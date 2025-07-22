on emacsclient(input)
	do shell script "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n -a \"/Applications/Emacs.app/Contents/MacOS/Emacs\" '" & input & "' && open -a Emacs"
end emacsclient

on open location input
	emacsclient(input)
end open location

on open inputs
	repeat with raw_input in inputs
		set input to POSIX path of raw_input
		emacsclient(input)
	end repeat
end open

on run
	do shell script emacsclient("")
	tell application "Emacs" to activate
end run