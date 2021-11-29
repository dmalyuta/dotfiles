import * as vscode from 'vscode';

// Globals
let prevTime = 0;
let retriggerInterval = 10;

// Wait for ms milliseconds
function delay(ms: number) {
    if (ms > 0) {
        return new Promise((resolve) => setTimeout(resolve, ms));
    }
}

async function multiSuggestionsCommand(interval: number = 300) {
	let currentTime = Date.now()/1000; // UNIX seconds now
	await vscode.commands.executeCommand('editor.action.triggerSuggest');
	if (currentTime - prevTime > retriggerInterval) {
		console.log('Re-trigger suggestions');
		await delay(interval);
		await vscode.commands.executeCommand('editor.action.triggerSuggest');
		prevTime = currentTime;
	}
}

// Runs at extension activation
export function activate(context: vscode.ExtensionContext) {

	console.log('Activated the SuggestionsMultiTrigger extension');

	let triggerSuggestionCmd = vscode.commands.registerCommand(
		'suggestions-multi-trigger.toggleSuggestions',
		multiSuggestionsCommand);

	context.subscriptions.push(triggerSuggestionCmd);
}

// Runs at extension deactivation
export function deactivate() {}