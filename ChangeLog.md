# 0.3.0.0
- Include timezone and accuracy in location results

# 0.2.0.1
- Fixed a problem with correct decoding of 28-bit offsets

# 0.2.0.0
- Changed interface to normal Either String GeoResult

# 0.1.0.3 -> 0.1.0.4
- Removed lru
- Switched from binary to cereal with a significant speedup

# 0.1.0.2 -> 0.1.0.3
- Changed bytestring-mmap to mmap
- Add LRU caching to improve performance (10000 entries are cached)

# 0.1.0.1 -> 0.1.0.2

- GHC-7.10 compatibiliy
- Add Changelog.md
- List Changelog.md and README.md in cabal's extra-source-files
- add stack.yaml
