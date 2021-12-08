import { type } from 'os';
import { types } from 'util';
import * as vscode from 'vscode';

// Globals
let prevTime: number = 0;
let defaultIntervalMilliseconds: number = 300;
let defaultRetriggerIntervalSeconds: number = 100;
let defaultLanguages: string[] = [];

// Wait for ms milliseconds
function delay(ms: number) {
    if (ms > 0) {
        return new Promise((resolve) => setTimeout(resolve, ms));
    }
}

async function multiSuggestionsCommand(arg: any) {
	// Set the arguments
	let interval = defaultIntervalMilliseconds;
	let retriggerIntervalSeconds = defaultRetriggerIntervalSeconds;
	let languages = defaultLanguages;
	if (arg !== null && typeof arg === 'object') {
		if (!(arg.delay === undefined) && typeof arg.delay === 'number') {
			interval = arg.delay;
		}
		if (!(arg.interval === undefined) && typeof arg.interval === 'number') {
			retriggerIntervalSeconds = arg.interval;
		}
		if (!(arg.languages === undefined) && typeof arg.languages === 'object') {
			languages = arg.languages;
		}
	}
	console.log(interval);
	console.log(retriggerIntervalSeconds);
	console.log(languages);
	const editor = vscode.window.activeTextEditor;
	if (editor) {
		let language = editor.document.languageId;
		if (languages.includes(language)) {
			// Re-trigger suggestions for Python
			let currentTime: number = Date.now()/1000; // UNIX seconds now
			await vscode.commands.executeCommand('editor.action.triggerSuggest');
			if (currentTime - prevTime > retriggerIntervalSeconds) {
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
