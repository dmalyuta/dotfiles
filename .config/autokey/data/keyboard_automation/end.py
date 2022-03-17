winClass = window.get_active_class()
if winClass not in ("code.Code", "emacs.Emacs"):
    # Regular window
    keyboard.send_keys('<end>')
else:
    # VS Code
    keyboard.send_keys('<alt>+e')