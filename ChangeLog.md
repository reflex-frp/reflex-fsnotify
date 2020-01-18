# Revision history for reflex-fsnotify

## 0.2.0.0

* Deprecate `watchDirectory`
* Add `watchDir` and `watchTree` corresponding to the functions in `System.FSNotify`
* Add `watchDirectoryTree`, an alternative to `watchTree` that tries to break symlink cycles

## 0.1.0.0

* Initial release containing `watchDirectory`.
