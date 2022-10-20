# CI Workflows

## Overview

- [Build](build.yml)
  - Build and test Haskell code
- [Draft](draft.yml)
  - Create a GH draft release with the source code and license file
- [Release](release.yml)
  - Upload the package and docs to Hackage

## Events

```mermaid
graph LR
    event[GH Event]-->|on push|Build
    event-->|tag created|Draft
    Draft-->|create draft release|End
    event-->|release published|Release
    Release-->|upload artifacts to Hackage|End
    Build-->End
```
