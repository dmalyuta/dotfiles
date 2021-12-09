import * as vscode from 'vscode';

// Globals
let defaultFirstIntervalMilliseconds: number = 300;
let defaultRetriggerIntervalSeconds: number = 100;
let defaultLanguages: string[] = [];
let dontRetrigger: boolean = false;
const afterFirstTriggerContext: string = 'suggestions-multi-trigger.afterFirstTrigger';
let prevTime: number = 0;

// Wait for ms milliseconds
function delay(ms: number) {
    if (ms > 0) {
        return new Promise((resolve) => setTimeout(resolve, ms));
    }
}

async function doMultiSuggestions(arg: any) {
	// Set the arguments
	let interval = defaultFirstIntervalMilliseconds;
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

	// Run the suggestions trigger command
	const editor = vscode.window.activeTextEditor;
	if (editor) {
		let language = editor.document.languageId;
		if (languages.includes(language)) {
			// Re-trigger suggestions for the special languages
			let currentTime: number = Date.now()/1000; // UNIX seconds now
			let cursorPos: number = editor.document.offsetAt(editor.selection.active);
			await vscode.commands.executeCommand('editor.action.triggerSuggest');
			if (currentTime - prevTime > retriggerIntervalSeconds) {
				console.log('Re-trigger suggestions');
				vscode.commands.executeCommand('setContext', afterFirstTriggerContext, true);
				await delay(interval);
				if (editor.document.offsetAt(editor.selection.active) !== cursorPos) {
					cancelCompletion();
				}
				if (!dontRetrigger) {
					await vscode.commands.executeCommand('editor.action.triggerSuggest');
				}
				vscode.commands.executeCommand('setContext', afterFirstTriggerContext, false);
			}
			prevTime = currentTime;
		} else {
			// Simply trigger suggestions for other languages
			await vscode.commands.executeCommand('editor.action.triggerSuggest');
		}
	}
	dontRetrigger = false;
}

async function cancelSecondTrigger() {
	dontRetrigger = true;
}

async function cancelCompletion(event?: vscode.TextDocumentChangeEvent) {
	cancelSecondTrigger();
	vscode.commands.executeCommand('hideSuggestWidget');
}

// Runs at extension activation
export function activate(context: vscode.ExtensionContext) {

	console.log('Activated the SuggestionsMultiTrigger extension');

	let triggerSuggestionCmd = vscode.commands.registerCommand(
		'suggestions-multi-trigger.toggleSuggestions', doMultiSuggestions);
	context.subscriptions.push(triggerSuggestionCmd);

	let cancelRetriggerCmd = vscode.commands.registerCommand(
		'suggestions-multi-trigger.cancelSecondTrigger', cancelSecondTrigger);
	context.subscriptions.push(cancelRetriggerCmd);

	let cancelCompletionCmd = vscode.commands.registerCommand(
		'suggestions-multi-trigger.cancelCompletion', cancelCompletion);
	context.subscriptions.push(cancelCompletionCmd);

	vscode.workspace.onDidChangeTextDocument(cancelCompletion);
}

// Runs at extension deactivation
export function deactivate() {}
