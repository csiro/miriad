# Post-Installation Instructions for Miriad

After extracting the Miriad distribution tarball, you should run the post-install script to complete setup:

```bash
./scripts/post_install.sh
```

This script will:
- Generate the `MIRRC.sh` file with the correct installation path.
- Remove macOS quarantine attributes that may prevent executables from running.

Once you've run the script, set up your environment by sourcing `MIRRC.sh`:

```bash
. ./MIRRC.sh
```

You’re now ready to use Miriad!