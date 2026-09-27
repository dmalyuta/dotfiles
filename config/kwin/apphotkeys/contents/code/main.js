// Hotkeys that focus an app, or launch it if it has no window.
//
// Focusing picks the app's best window across all virtual desktops, switching desktop and
// un-minimizing as needed. Launching invokes kglobalaccel's "_launch" action of the app's desktop
// entry, so the app starts just like from the app menu. That action only exists once the entry is
// registered with kglobalaccel, which start_fresh.sh does.
//
// The keys here are defaults: start_fresh.sh sets the same ones, and they can be changed in System
// Settings > Keyboard > Shortcuts > KWin.

// Desktop entry to launch, and the ids that identify the app's windows (lowercase resource class,
// resource name or desktop file name).
const apps = {
    brave: { desktop: "brave-browser.desktop", ids: ["brave-browser"] },
    kitty: { desktop: "kitty.desktop", ids: ["kitty"] },
    dolphin: { desktop: "org.kde.dolphin.desktop", ids: ["org.kde.dolphin"] },
    fsearch: { desktop: "io.github.cboxdoerfer.FSearch.desktop", ids: ["io.github.cboxdoerfer.fsearch"] },
    pureref: { desktop: "pureref.desktop", ids: ["pureref"] },
    obsidian: { desktop: "obsidian.desktop", ids: ["md.obsidian.obsidian"] },
    inkscape: { desktop: "org.inkscape.Inkscape.desktop", ids: ["org.inkscape.inkscape"] },
    speedcrunch: { desktop: "speedcrunch.desktop", ids: ["org.speedcrunch.speedcrunch"] },
    pdfxchange: { desktop: "pdf-xchange-editor.desktop", ids: ["pxceditor.exe"] },
    vscode: { desktop: "com.microsoft.VSCode.desktop", ids: ["com.microsoft.vscode"] },
};

// VS Code workspaces for the Meta+C, Meta+<key> sequence. start_fresh.sh keeps the list and passes
// it in the script's config as "<key>=<folder path>;...". It also writes the desktop entry that
// opens each folder. A workspace's window is found by the folder name in its title.
const vscodeWorkspaces = String(readConfig("vscodeWorkspaces", "")).split(";").filter(s => s)
    .map(entry => {
        const split = entry.indexOf("=");
        const key = entry.slice(0, split);
        return {
            key: key,
            folder: entry.slice(split + 1).split("/").pop(),
            desktop: "vscode-workspace-" + key.toLowerCase() + ".desktop",
        };
    });

function callKGlobalAccel(component, action) {
    // kglobalaccel's object path is the component name with non-alphanumerics turned into "_".
    const path = "/component/" + component.replace(/[^A-Za-z0-9]/g, "_");
    callDBus("org.kde.kglobalaccel", path, "org.kde.kglobalaccel.Component", "invokeShortcut",
        action);
}

function launch(app) {
    callKGlobalAccel(app.desktop, "_launch");
}

function isOnCurrentDesktop(window) {
    const current = workspace.currentDesktop;
    return window.onAllDesktops || window.desktops.some(d => d.id === current.id);
}

// The app's best window, or null: one on the current desktop beats one elsewhere, and a shown one
// beats a minimized one. Ties go to the topmost, i.e. most recently used, window.
function findWindow(app, captionPattern) {
    let best = null;
    let bestScore = -1;
    const windows = workspace.stackingOrder;
    for (let i = windows.length - 1; i >= 0; i--) {
        const w = windows[i];
        if (!w.normalWindow || w.skipTaskbar) {
            continue;
        }
        const ids = [w.resourceClass, w.resourceName, w.desktopFileName].map(s => s.toLowerCase());
        if (!app.ids.some(id => ids.includes(id))) {
            continue;
        }
        if (captionPattern && !captionPattern.test(w.caption)) {
            continue;
        }
        const score = (isOnCurrentDesktop(w) ? 2 : 0) + (w.minimized ? 0 : 1);
        if (score > bestScore) {
            best = w;
            bestScore = score;
        }
    }
    return best;
}

function activate(window) {
    if (!isOnCurrentDesktop(window)) {
        workspace.currentDesktop = window.desktops[0];
    }
    window.minimized = false;
    workspace.activeWindow = window;
}

function focusOrLaunch(app, captionPattern) {
    const window = findWindow(app, captionPattern);
    if (window) {
        activate(window);
    } else {
        launch(app);
    }
}

function hotkey(name, keys, callback) {
    registerShortcut("App Hotkeys: " + name, "App Hotkeys: " + name, keys, callback);
}

hotkey("Brave", "Meta+B", () => focusOrLaunch(apps.brave));
hotkey("New Brave window", "Meta+Shift+B", () => launch(apps.brave));
hotkey("Terminal", "Meta+W", () => focusOrLaunch(apps.kitty));
hotkey("New terminal", "Meta+Shift+W", () => launch(apps.kitty));
hotkey("Dolphin", "Meta+E", () => focusOrLaunch(apps.dolphin));
hotkey("New Dolphin window", "Meta+Shift+E", () => launch(apps.dolphin));
hotkey("FSearch", "Meta+S", () => focusOrLaunch(apps.fsearch));
hotkey("PureRef", "Meta+R", () => focusOrLaunch(apps.pureref));
hotkey("Obsidian", "Meta+O", () => focusOrLaunch(apps.obsidian));
hotkey("Inkscape", "Meta+I", () => focusOrLaunch(apps.inkscape));
hotkey("SpeedCrunch", "Meta+N", () => focusOrLaunch(apps.speedcrunch));
hotkey("PDF-XChange Editor", "Meta+P", () => focusOrLaunch(apps.pdfxchange));

// Meta+C focuses VS Code right away. Within the next second, Meta+C again shows all VS Code windows
// to pick from, while Meta+<key> focuses that workspace's window, opening it if needed.
const sequenceMs = 1000;
let vscodePressedAt = 0;

function takeVscodeSequence() {
    const active = Date.now() - vscodePressedAt < sequenceMs;
    vscodePressedAt = 0;
    return active;
}

hotkey("VS Code", "Meta+C", () => {
    if (takeVscodeSequence()) {
        callKGlobalAccel("kwin", "ExposeClass");
        return;
    }
    vscodePressedAt = Date.now();
    focusOrLaunch(apps.vscode);
});

for (const workspaceInfo of vscodeWorkspaces) {
    const app = { desktop: workspaceInfo.desktop, ids: apps.vscode.ids };
    // Titles look like "file.py - WorkspaceTitle - Visual Studio Code".
    const folder = workspaceInfo.folder.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    const title = new RegExp("(^| - )" + folder + "( - |$)");
    // Named by the letter, which start_fresh.sh relies on to drop the shortcuts of removed ones.
    registerShortcut("App Hotkeys: VS Code workspace " + workspaceInfo.key,
        "App Hotkeys: VS Code " + workspaceInfo.folder, "Meta+" + workspaceInfo.key, () => {
            if (takeVscodeSequence()) {
                focusOrLaunch(app, title);
            }
        });
}
