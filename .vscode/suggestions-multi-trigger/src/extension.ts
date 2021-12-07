import * as vscode from 'vscode';

// Globals
let prevTime: number = 0;
let retriggerInterval: number = 180; // seconds

// Wait for ms milliseconds
function delay(ms: number) {
    if (ms > 0) {
        return new Promise((resolve) => setTimeout(resolve, ms));
    }
}

async function multiSuggestionsCommand(interval: number = 300) {
	const editor = vscode.window.activeTextEditor;
	if (editor) {
		let language = editor.document.languageId;
		if (language === 'python') {
			// Re-trigger suggestions for Python
			let currentTime: number = Date.now()/1000; // UNIX seconds now
			await vscode.commands.executeCommand('editor.action.triggerSuggest');
			if (currentTime - prevTime > retriggerInterval) {
				console.log('Re-trigger suggestions');
				await delay(interval);
				await vscode.commands.executeCommand('editor.action.triggerSuggest');
			}
			prevTime = currentTime;
		} else {
			// Simply trigger suggestions for other languages
			await vscode.commands.executeCommand('editor.action.triggerSuggest');
		}
	}
}

// Runs at extension activation
export function activate(context: vscode.ExtensionContext) {

	console.log('Activated the SuggestionsMultiTrigger extension');

	// vscode.workspace.onDidSaveTextDocument(() => {
	//     triggerTwice = true;
	// });

	let triggerSuggestionCmd = vscode.commands.registerCommand(
		'suggestions-multi-trigger.toggleSuggestions',
		multiSuggestionsCommand);

	context.subscriptions.push(triggerSuggestionCmd);
}

// Runs at extension deactivation
export function deactivate() {}
