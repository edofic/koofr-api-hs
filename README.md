# Koofr API for Haskell
[![Build Status](https://travis-ci.org/edofic/koofr-api-hs.png?branch=master)](https://travis-ci.org/edofic/koofr-api-hs)

Koofr is a storage as a service provider located in Europe. See https://koofr.eu for more info. This is an API wrapper that simplifies working with files. 
The API is contained in a type class in order to allow mocking. Mock runner and wider API coverage coming soon.

### Mounts
Mounts are the central concept to Koofr. Each mount is a virtual filesystem root; it may be a physical device, a shared folder or something else. Each mount has a unique identifier to reference it.

A mount may contain other mounts. For example: you have a storage device *My Place* where you have a folder *Pictures*. If you share *Pictures* you will implicitly create a new mount.  So a picture stored under `My Place | /Pictures/01.jpg` will also be accessible through `Pictures | /01.jpg`.

### Files
Each file is identified by a pair of mount identifier and a path. Therefore all file operations take a mount id (to specify which filesystem root to use) and a path.

Files stored in Koofr are immutable. This means you cannot modify file after you upload it. You can however delete it and replace it with a modified version - koofr will detect this as a modification.

### Authentication 
TODO

## Example
TODO

## Building
The recommended way to use this library is by depending it its [hackage package](http://hackage.haskell.org/package/koofr-client) although you may build it yourself using `cabal`.