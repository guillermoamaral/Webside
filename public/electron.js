const { app, BrowserWindow, ipcMain } = require("electron");
const path = require("path");

function createWindow() {
    const win = new BrowserWindow({
        width: 1920,
        height: 1080,
        frame: false,
        titleBarStyle: "hidden",
        autoHideMenuBar: true,
        webPreferences: {
            nodeIntegration: true,
            contextIsolation: false,
        },
    });
    const isDev = true;
    const url = isDev
        ? "http://localhost:3000"
        : `file://${path.join(__dirname, "../build/index.html")}`;
    win.loadURL(url);

    // Handle window control events
    ipcMain.handle('minimize-window', () => {
        win.minimize();
    });

    ipcMain.handle('maximize-window', () => {
        win.maximize();
    });

    ipcMain.handle('restore-window', () => {
        win.unmaximize();
    });

    ipcMain.handle('close-window', () => {
        win.close();
    });
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
