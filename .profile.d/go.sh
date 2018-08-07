CONFIGURE_GO=no
if [ -x /usr/bin/go ]
then
	CONFIGURE_GO=yes
elif [ -x /usr/local/bin/go ]
then
	CONFIGURE_GO=yes
elif [ -x /usr/local/go/bin/go ]
then
	CONFIGURE_GO=yes
	PATH=$PATH:/usr/local/go/bin
fi

if [ "$CONFIGURE_GO" = "yes" ]
then
	export GOPATH=$HOME
	
	alias gob='go build'
	alias gog='go get'
	alias gin='go install'
	alias gf='go fmt'
	alias god='godoc'
	alias got='go test'
	alias goc='go clean'
	alias gocr='go clean ./...'
	alias gobr='go build ./...'
	alias ginr='go install ./...'
	alias gotr='go test ./...'
fi

unset CONFIGURE_GO
