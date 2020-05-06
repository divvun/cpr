use std::path::PathBuf;

pub(crate) fn get_kits_path() -> Option<PathBuf> {
    #[cfg(windows)]
    {
        use std::io;
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
                    .map(|r| r.map(|r| r.file_name()))
                    .collect::<Result<Vec<_>, _>>()?;
                for v in versions {
                    let include_path = folder.join(&v);
                    let check_path = include_path.join("um").join("winsdkver.h");
                    if check_path.exists() {
                        log::debug!("Found Windows 10 kit version {:?}", v);
                        return Ok(include_path);
                    }
                }
                Ok(folder)
            };

            match f() {
                Ok(s) => return Some(s),
                Err(e) => {
                    log::debug!("Windows 10 Kit not found in {:?}: {:#}", path, e);
                }
            }
        }

        None
    }

    #[cfg(not(windows))]
    {
        std::env::var_os("CPR_KITS_PATH").map(Into::into)
    }
}

pub(crate) fn get_msvc_path() -> Option<PathBuf> {
    #[cfg(windows)]
    {
        use std::io;

        let pf86 = match std::env::var("ProgramFiles(x86)") {
            Ok(res) => PathBuf::from(res),
            Err(_) => {
                log::debug!("No value for ProgramFiles(x86)");
                return None;
            }
        };

        let vswhere = pf86
            .join("Microsoft Visual Studio")
            .join("Installer")
            .join("vswhere.exe");

        let mut cmd = std::process::Command::new(vswhere);
        cmd.arg("-latest");
        let out = match cmd.output() {
            Ok(res) => res,
            Err(e) => {
                log::debug!("While running vswhere: {:?}", e);
                return None;
            }
        };

        let s = match std::str::from_utf8(&out.stdout[..]) {
            Ok(x) => x,
            Err(e) => {
                log::debug!("vswhere gave non-utf8 output: {:?}", e);
                return None;
            }
        };

        let prefix = "installationPath: ";
        for line in s.lines() {
            if line.starts_with(prefix) {
                let value = line.replace(prefix, "");
                let install_path = PathBuf::from(value);

                let tools_path = install_path.join("VC").join("Tools").join("MSVC");
                let f = || -> Result<_, io::Error> {
                    let versions = std::fs::read_dir(&tools_path)?
                        .map(|r| r.map(|r| r.file_name()))
                        .collect::<Result<Vec<_>, _>>()?;
                    for v in versions {
                        let include_path = tools_path.join(&v).join("include");
                        if include_path.exists() {
                            return Ok(Some(include_path));
                        }
                    }
                    Ok(None)
                };

                match f() {
                    Ok(x) => {
                        if let Some(x) = x {
                            return Some(x);
                        }
                    }
                    Err(e) => log::debug!("While looking for MSVC path: {:?}", e),
                }
            }
        }

        None
    }

    #[cfg(not(windows))]
    {
        std::env::var_os("CPR_MSVC_PATH").map(Into::into)
    }
}
