use super::{grammar::Include, SourceProvider};
use std::path::{Path, PathBuf};

pub struct FileSourceProvider {
    system_paths: Vec<PathBuf>,
    working_path: PathBuf,
}

impl FileSourceProvider {
    pub fn new(system_paths: Vec<PathBuf>, working_path: PathBuf) -> Self {
        Self {
            system_paths,
            working_path,
        }
    }

    fn resolve_system(&self, candidate: &Path) -> Option<PathBuf> {
        self.system_paths
            .iter()
            .map(|x| x.join(candidate))
            .find(|x| x.exists())
    }

    fn resolve_quoted(&self, candidate: &Path) -> Option<PathBuf> {
        Some(self.working_path.join(candidate))
            .into_iter()
            .find(|x| x.exists())
    }

    fn read(&self, path: PathBuf) -> Option<(PathBuf, String)> {
        // FIXME: swallowing I/O errors over here
        std::fs::read_to_string(&path).ok().map(|file| (path, file))
    }
}

impl SourceProvider for FileSourceProvider {
    fn resolve(&self, include: &Include) -> Option<(PathBuf, String)> {
        self.read(match include {
            Include::System(path) => self.resolve_system(path),
            Include::Quoted(path) => self
                .resolve_quoted(path)
                .or_else(|| self.resolve_system(path)),
        }?)
    }
}
