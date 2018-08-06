KEYCHAIN_RC="$HOME/.keychain/${HOST%%.*}-sh"
keychain -q
if [ -s "$KEYCHAIN_RC" ]
then
	source "$KEYCHAIN_RC"
fi
