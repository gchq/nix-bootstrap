# Contributing to nix-bootstrap

This file details the steps necessary in order to contribute to the project,
providing guidance and development practices to follow in order to make successful contributions.

## Contributions

Please read our [Code of Conduct](CODE_OF_CONDUCT.md) before contributing to this project.

Before your contributions can be accepted, you must:

- Sign the [GCHQ Contributor Licence Agreement](https://cla-assistant.io/gchq/nix-bootstrap).
- Push your changes to new branch.
- Submit a pull request.

## Coding Practices

### Versioning

`nix-bootstrap` broadly follows the [PVP Versioning Specification](https://pvp.haskell.org/),
but with some modifications on the basis it's an end product rather than a library:

- The flagship version (first component) is bumped only at major, defining releases.
- The major version (second component) is bumped whenever the user input changes - this is
  analagous to a breaking change.
- The minor version (third component) is bumped for all other features.
- The patch version (last component) is bumped for all other changes which don't affect
  nix-bootstrap's feature set.

The version number must be updated for every (set of) commits added to `main`.

### Comments

- When considering adding a comment, first consider if the code could be better explained by
  restructuring it (e.g. by pulling out a function)
- If the code cannot be made clear by restructuring, add a comment to explain it
- Haddock comments should be added to all exposed items in libraries

### Property Ordering

For the purpose of this point, a logical order is one of the following:

- Something universally recognisable, like alphabetical
- Something recognisable in context, such as CSS properties being grouped into width and height,
  flex options, colour options etc.
  - In this case, the groups should each be logically ordered

Based on those definitions:

- In cases where a logical order of properties is already being used, that order must be followed
  unless changing to a new logical order is justified
- In cases where no logical order is given already, developers touching that property set should re-order the set to fit a logical order
- New property sets must be given a logical order

### Use of Type Systems

- Type systems must be leveraged where possible to ensure code correctness

### Identifiers / Variable Names

Identifiers must be:

- Consistent
- Meaningful
- Given in line with the idioms/best practice of the language/toolchain in which it's written

## Version Control Practices

### Branching Strategies

- Rebase and Fast Forward will be the default merge strategy for PRs
- No merge commits from the base branch (e.g. `main`) will be permitted into feature branches.
  If conflicts arise, these should instead be solved by rebasing the feature branch onto the HEAD of the base branch

### Branch Naming

- The main branch will be called `main`
- Branch names should consist of the ticket number (where a ticket exists) and a short description
- Words in branch names should be separated by dashes

### Commit Conventions

Commits should be made as frequently as possible, and each commit should successfully build

### Commit Messages

Commit messages must:

- Be short where possible
- Be meaningful
- Have no reliance on external context (e.g. should not say things like "make PR changes")

### Pull Requests

Pull requests will undergo an in depth review by a project contributor to check the code
changes are compliant with our coding style.

This is a community so please be respectful of other members - offer encouragement, support and suggestions.

Please agree to the [GCHQ OSS Contributor License Agreement](https://cla-assistant.io/gchq/nix-bootstrap)
before submitting a pull request.
