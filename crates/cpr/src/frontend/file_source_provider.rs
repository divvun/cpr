use super::{
    grammar::Include, Error, FileId, FileInfo, FilePath, IdGenerator, SourceDir, SourceProvider,
};
use std::{collections::HashMap, path::Path};

pub struct FileSourceProvider {
    system_dirs: Vec<SourceDir>,
    path_to_id: HashMap<FilePath, FileId>,
    id_to_info: HashMap<FileId, FileInfo>,
}

impl FileSourceProvider {
    pub fn new(system_dirs: Vec<SourceDir>) -> Self {
        Self {
            system_dirs,
            path_to_id: Default::default(),
            id_to_info: Default::default(),
        }
    }

    fn resolve_from_dir(&self, dir: &SourceDir, candidate: &Path) -> Option<FilePath> {
        if let Ok(abs_path) = dir.path.join(candidate).canonicalize() {
            if let Ok(dir_path) = dir.path.canonicalize() {
                if let Ok(rel_path) = abs_path.strip_prefix(&dir_path) {
                    return Some(FilePath {
                        dir: dir.clone(),
                        rel_path: rel_path.into(),
                    });
                } else {
                    log::warn!(
                        "Found file {:?} but couldn't strip prefix {:?}",
                        abs_path,
                        dir.path
                    );
                }
            }
        }
        None
    }

    fn resolve_system(&self, candidate: &Path) -> Option<FilePath> {
        self.system_dirs
            .iter()
            .find_map(|dir| self.resolve_from_dir(dir, candidate))
    }
}

impl SourceProvider for FileSourceProvider {
    fn resolve(
        &mut self,
        idgen: &mut IdGenerator,
        working_dir: &SourceDir,
        include: &Include,
    ) -> Result<FileId, Error> {
        let path = match include {
            Include::System(path) => self.resolve_system(path),
            Include::Quoted(path) => self
                .resolve_from_dir(working_dir, path)
                .or_else(|| self.resolve_system(path)),
        }
        .ok_or_else(|| Error::NotFound(include.clone()))?;

        let id = self.path_to_id.get(&path).copied().unwrap_or_else(|| {
            let id = idgen.generate_id();
            let info = FileInfo {
                id,
                path: path.clone(),
            };
            self.path_to_id.insert(path, id);
            self.id_to_info.insert(id, info);
            id
        });
        Ok(id)
    }

    fn info(&self, id: FileId) -> Option<&FileInfo> {
        self.id_to_info.get(&id)
    }

    fn read(&self, id: FileId) -> Result<String, Error> {
        let info = self.id_to_info.get(&id).ok_or(Error::UnknownFileId)?;
        Ok(std::fs::read_to_string(&info.path.source_path())?)
    }
}
