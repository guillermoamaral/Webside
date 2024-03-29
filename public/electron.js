const { app, BrowserWindow } = require("electron");
const path = require("path");

function createWindow() {
	const win = new BrowserWindow({
		width: 1920,
		height: 1080,
		webPreferences: {
			nodeIntegration: true,
		},
	});
	// const url = isDev
	// 	? "http://localhost:8000"
	// 	: `file://${path.join(__dirname, "../build/index.html")}`;
	win.loadURL(`file://${path.join(__dirname, "../build/index.html")}`);
}

app.on("ready", createWindow);

app.on("window-all-closed", function () {
	if (process.platform !== "darwin") {
		app.quit();
	}
});

app.on("activate", function () {
	if (BrowserWindow.getAllWindows().length === 0) createWindow();
});
