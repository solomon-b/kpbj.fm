# KPBJ 95.9FM

Community radio station website built with Haskell/Servant/HTMX.

## Deployment

The project uses automated CI/CD with GitHub Actions and Fly.io.

### Environments

| Environment | URL                             | Deploys From         |
|-------------|---------------------------------|----------------------|
| Staging     | https://kpbj-fm-staging.fly.dev | Every push to `main` |
| Production  | https://kpbj.fm                 | Git tags (`v*`)      |

### Image Tagging Strategy

| Trigger        | Image Tag              | Example              |
|----------------|------------------------|----------------------|
| Push to `main` | `sha-<commit>`         | `sha-abc1234def5678` |
| Release tag    | `<version>` + `latest` | `0.3.2`, `latest`    |

### Staging Deployments

Staging deploys automatically on every push to `main`.

```
Push to main → Build sha-abc1234 → Deploy to staging
```

### Production Releases

To release a new version to production:

```bash
just release-pr 0.3.2
```

This command:
1. Creates a `release/0.3.2` branch
2. Updates the cabal version to `0.3.2`
3. Opens a PR to `main`

When you merge the PR:
1. GitHub Action automatically creates the `v0.3.2` git tag
2. Tag push triggers the production deployment
3. Image is tagged as both `0.3.2` and `latest`

### Commands

View deployment status:
```bash
just staging-status    # Staging
just status            # Production
```

View logs:
```bash
just staging-logs      # Staging
just prod-logs         # Production
```

SSH into running container:
```bash
just staging-ssh       # Staging
just prod-ssh          # Production
```

### Local Docker Builds

For local testing:
```bash
# Build with cabal version as tag
nix build .#docker

# Build with custom tag
IMAGE_TAG=my-test nix build .#docker --impure
```

## Development

### Setup

```bash
# Enter dev shell
nix develop

# Start database
just postgres-dev-start

# Run migrations
just migrations-run

# Load mock data (optional)
just mock-data

# Start server
just run
```

### Common Commands

```bash
just build              # Build all packages
just test               # Run tests
just format             # Format code (ormolu + nixpkgs-fmt)
just format-changed     # Format only changed files
just hlint              # Lint Haskell code
```
