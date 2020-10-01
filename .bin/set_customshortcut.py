import subprocess
import sys

# defining keys & strings to be used
key = "org.gnome.settings-daemon.plugins.media-keys custom-keybindings"
subkey1 = key.replace(" ", ".")[:-1]+":"
item_s = "/"+key.replace(" ", "/").replace(".", "/")+"/"
firstname = "custom"
# get the current list of custom shortcuts
get = lambda cmd: subprocess.check_output(["/bin/bash", "-c", cmd]).decode("utf-8")
array_str = get("gsettings get "+key)
# in case the array was empty, remove the annotation hints
command_result = array_str.lstrip("@as")
current = eval(command_result)
# check if a shortcut with this name already exists
shortcut_name = sys.argv[1]
already_exists = False
for binding in current:
    binding_name = get("gsettings get org.gnome.settings-daemon.plugins."
                       "media-keys.custom-keybinding:"+binding+" name")[1:-2]
    if binding_name==shortcut_name:
        already_exists = True
        break

if not already_exists:
    # make sure the additional keybinding mention is no duplicate
    n = 1
    while True:
        new = item_s+firstname+str(n)+"/"
        if new in current:
            n = n+1
        else:
            break
    # add the new keybinding to the list
    current.append(new)
    # create the shortcut, set the name, command and shortcut key
    cmd0 = 'gsettings set '+key+' "'+str(current)+'"'
    cmd1 = 'gsettings set '+subkey1+new+" name '"+shortcut_name+"'"
    cmd2 = 'gsettings set '+subkey1+new+" command '"+sys.argv[2]+"'"
    cmd3 = 'gsettings set '+subkey1+new+" binding '"+sys.argv[3]+"'"

    for cmd in [cmd0, cmd1, cmd2, cmd3]:
        subprocess.call(["/bin/bash", "-c", cmd])

