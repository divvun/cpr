use std::io;
use std::path::PathBuf;

pub(crate) fn detect_path() -> Option<PathBuf> {
    #[cfg(windows)]
    {
        use winreg::enums::*;
        use winreg::RegKey;

        let paths = [
            (HKEY_LOCAL_MACHINE, r"SOFTWARE\WOW6432Node"),
            (HKEY_CURRENT_USER, r"SOFTWARE\WOW6432Node"),
            (HKEY_LOCAL_MACHINE, r"SOFTWARE"),
            (HKEY_CURRENT_USER, r"SOFTWARE"),
        ];
        let suffix = r"Microsoft\Microsoft SDKs\Windows\v10.0";

        for (root, prefix) in &paths {
            let root = RegKey::predef(*root);
            let path = format!(r"{}\{}", prefix, suffix);

            let f = || -> Result<_, io::Error> {
                let key = root.open_subkey(&path)?;
                let folder: String = key.get_value("InstallationFolder")?;
                let folder = PathBuf::from(folder).join("Include");

                let versions = std::fs::read_dir(&folder)?
                    .map(|r| r.map(|r| r.path()))
                    .collect::<Result<Vec<_>, _>>()?;
                for v in versions {
                    let include_path = folder.join(&v);
                    let check_path = include_path.join("um").join("winsdkver.h");
                    if check_path.exists() {
                        return Ok(include_path);
                    }
                }
                Ok(folder)
            };

            match f() {
                Ok(s) => return Some(s),
                Err(e) => {
                    log::trace!("Windows 10 Kit not found in {:?}: {:#}", path, e);
                }
            }
        }

        None
    }

    #[cfg(not(windows))]
    {
        None
    }
}
