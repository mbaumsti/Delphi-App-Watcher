# Build Instructions for AppWatcher

## ✨ How to Open and Organize the AppWatcher Projects

Since `.dproj` and `.groupproj` files are no longer distributed, please follow these steps to rebuild the environment:

### 1. Open the Projects Individually

- Open Delphi.
- Open the following files manually:
  - `AppWatcherMaster_main.dpr` (Master Server)
  - `AppWatcherAgent_main.dpr` (Agent Service)
  - `AppWatcherClient_Component.dpk` (Client Component Package)
  - `AppWatcher2Agent_main.dpr` (Agent Launcher Stub)

*(Adjust if your version names differ slightly)*

### 2. (Optional) Create a Project Group

- In Delphi: `File` > `New` > `Project Group`
- Then: `File` > `Add Existing Project...` for each `.dpr` or `.dpk` opened above.
- Save this project group locally if you want (e.g., `AppWatcher.groupproj`).

### 3. Configure Delphi Library Paths

You must add the AppWatcher sources to your **Library Path**:

- Open: `Tools` > `Options` > `Language` > `Delphi Options` > `Library`
- Under **Library Path**, add the folders where the following source files are located:
  - Core AppWatcher source units
  - Named Pipes (PipesCommon.pas, PipeClient.pas, PipeServer.pas)

### 4. Install the AppWatcher Client Package

- Open `AppWatcherClientPackage.dpk`
- Right-click > `Compile`
- Right-click > `Install`

*(Make sure you use the correct package for your Delphi version, as explained in the README.)*

### 5. You're ready!

You can now compile and test all the components: Master, Agent, Client.

---

If you encounter any issues during build or installation, please check the latest [README](README.md) or [Changelog](CHANGELOG.md) for updates.

Thank you for using AppWatcher! 🚀

