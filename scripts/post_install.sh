MIRIAD_ROOT="$(pwd)"
if [ ! -f "${MIRIAD_ROOT}/post_install.sh" ]
then
    echo ". post_install.sh should be run from the directory it is located"
else
    TEMPLATE="${MIRIAD_ROOT}/scripts/MIRRC.sh.in"
    OUTPUT="${MIRIAD_ROOT}/MIRRC.sh"

    echo "Generating MIRRC.sh with install path: $MIRIAD_ROOT"

    sed "s|@MIRROOT@|$MIRIAD_ROOT|" "$TEMPLATE" > "$OUTPUT"

    echo "Removing quarantine attribute..."
    xattr -r -d com.apple.quarantine "$MIRIAD_ROOT" || echo "No quarantine attribute found or not on macOS."

    echo "MIRRC.sh created at: $OUTPUT"
fi