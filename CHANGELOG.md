# Changelog  

### 🛠 v3.1.2 – 2025-04-26

### ✨ Added
- **Full Delphi 10.2 compatibility validation**
  - Verified all main units (`AppWatcherMaster_main.pas`, `AppWatcherMaster_Deploy.pas`, `AppWatcherMaster_Backup.pas`) against Delphi 10.2 Tokyo.
  - Dynamically activated `StringGrid` options (`goFixedColClick`, `goFixedRowClick`, etc.) only in Delphi 10.4+ via `FormCreate`.
  - Corrected the `OnDrawCell` event signature to use `Integer` instead of `LongInt`.
- **Separation of runtime packages (`.dpk`)**
  - `AppWatcherClientPackage_Pre104.dpk` created for Delphi 10.2 and 10.3 without `{$LIBSUFFIX 'Auto'}`.
  - `AppWatcherClientPackage.dpk` maintained for Delphi 10.4 and above, using `{$LIBSUFFIX 'Auto'}`.

### 🐛 Fixed
- **Potential startup crash on Delphi 10.2**
  - Removal of non-supported options in `.dfm` files.
  - Ensured proper event handler binding for `TStringGrid` and `TListView`.

### 🔄 Upgrade Notes
1. If you experience project loading errors in Delphi 10.2 (`Property X does not exist`), ensure you refresh `.dfm` files.
2. If you had customized grid or list components, validate the event signatures (`OnDrawCell`, etc.).
3. Use the correct `.dpk` depending on your Delphi version:
   - **Delphi 10.2 / 10.3** ➔ `AppWatcherClientPackage_Pre104.dpk`
   - **Delphi 10.4+** ➔ `AppWatcherClientPackage.dpk`

---

### 🛠 v3.1.1 – 2025-04-24

### ✨ Added
- **Compiler compatibility improvements**
  - Conditional use of `{$LIBSUFFIX 'Auto'}` in `.dpk` to support Delphi 10.4+ only.
  - Added `System.Types` to `AppWatcher_Lang.pas` to support `TStringDynArray` in Delphi 10.2.
  
### 🐛 Fixed
- **Delphi 10.2.3 build issues**
  - Removed unsupported `StringGridApp.BeginUpdate` / `EndUpdate` in `AppWatcherMaster_main.pas` for Delphi < 11.
  - Validated builds from Delphi 10.2 Tokyo to Delphi 12 Athens.

### 🔄 Upgrade Notes
1. If you're using Delphi 10.2, re-compile the project — compatibility patches are now active.
2. Check that any custom grid logic does not rely on `BeginUpdate/EndUpdate` unless you use a component that provides it (e.g. `TMS`, `VirtualStringTree`, etc.).

---

### 🚀 v3.1.0 – 2025-04-23

### ✨ Added
- **Deployment with rotation**
  - `AppWatcherMaster_Deploy` can now keep an **automatically rotated backup history** of deployed versions.
  - New `[Backup]` section in `AppWatcher.ini`  
    | Key | Description | Example |
    |-----|-------------|---------|
    | `MaxVersions` | Maximum number of backups to retain | `2` |
    | `Folder` | Backup destination (leave blank = EXE directory) | `V:\Backup` |
  - When `MaxVersions` is reached, the oldest backup is removed before the next deployment.

- **Multi-version compatibility – `TAWStringArray`**
  - Conditional alias in `AppWatcher_Lang.pas`  
    - Delphi 12 + : `TArray<string>`  
    - Delphi ≤ 11 : `TStringDynArray`
  - Eliminates **E2010 “Incompatible types”** across all supported Delphi versions.

### 🐞 Fixed
- **README**: download link now points to `/releases/latest`, preventing 404 errors after a new tag is published.

### 🔄 Upgrade Notes
1. **Master only:** ensure `Folder` is writable when rotation is enabled.
2. Re-compile all projects so the new `TAWStringArray` alias is available.

---

## ⚙️ v3.0.0 - 09/04/2025

### ✨ Added
- **🧩 New utility: `AppWatcherStub`**
  - Lightweight stub that connects to the master and restarts the Agent executable on demand.
  - Useful during Agent updates or replacements.

### 🔄 Changed
- **🔁 Replaced TCP/IP with Named Pipes for Client ↔ Agent communication**
  - Improved reliability and performance, especially for local inter-process communication.
  - Introduced new internal architecture based on the library [NamedPipesForDelphi](https://github.com/superflexible/NamedPipesForDelphi).
  - The original code has been modularized into:
    - `PipesCommon`: shared types/constants/utilities
    - `PipeClient`: client implementation
    - `PipeServer`: server implementation
- ** Added a silent mode ** to stop and restart the application without warning the user

### 🛠 Internal Refactoring
- Legacy Indy TCP/IP logic removed from Client and Agent communication code.
- Added clean and asynchronous pipe-based event handling for both sides.

### 🧠 Notes
- This change **requires** that all components are updated together (Master, Agent, Client).
- The rest of the AppWatcher protocol remains compatible with v2.0.0.

---

## 🔄 v2.0.1 - 10/03/2025  

### 🛠 Fixed  
- **Bug in item deletion after sorting (`DeleteCopyItem`)**  
  - When sorting the list, the deletion was incorrect because the displayed index didn't match the actual index in `FCopyList`.  
  - A new function `GetRealIndexFromSelected` now retrieves the correct index before deleting or modifying items.  

### 🚀 Improved  
- **Code refactoring: Centralized real index lookup**  
  - The `GetRealIndexFromSelected` function is now used in multiple places (`DeleteCopyItem`, `AppListDblClick`, `BtnDelClick`), avoiding duplicate code.  
  - This ensures correct behavior even after sorting.  


### 🔄 Upgrade Notes  
- This update **does not break compatibility** with `2.0.0`.  
- If you were affected by **deletion issues after sorting**, upgrading is highly recommended.  

---

## 🚀 v2.0.0 - 09/03/2025  

### ⚠️ Breaking Changes  
- **`TAppWatcherMessage` structure updated**:  
  - `Handle` is now a `UInt64` instead of `HWnd` for **better compatibility across 32/64-bit systems**.  
  - **Record become Packed record**, modifying the message memory layout.  
  - ⚠ **Older versions will not be able to communicate with v2.0.0**.  
- **Protocol version update required**: Ensure **all components (Master, Agent, Client)** use the **same version**.  

### ✨ Added  
- **🚀 Application Deployment Management (`AppWatcherMaster_Deploy.pas`)**  
  - New feature to **deploy applications**.  
  - Allows **selecting applications to copy** from a source directory to destination paths.  
  - Fully integrated with the Master UI.  

- **📂 Application list sorting & filtering**  
  - Click on column headers to **sort** application lists.  
  - Toggle filtering to display **only applications marked for deployment**.  

- **🖥️ Auto-selection of added applications**  
  - When adding an application to the deployment list, it is **automatically selected** in the UI.  


### 🛠 Fixed & Improved  
- **Master now verify its role regularly** `Check if the MASTER is still the MASTER`.  
- **Agent Fixes:**  
  - `HandleCLIENTAckReply` now correctly **transmits parameters** (`StopDialog.Params := Msg.Params`).  
  - **INI file is now only reloaded if modified** to prevent unnecessary reads.  
- **Component Improvements:**  
  - `FindParentForm` logic refined for better UI hierarchy detection.  
  - `FLastCommand` is now properly set when commands are executed.  
- **Client Example Enhancements:**  
  - Uses `getCmdName` for better command interpretation.  
  - Added **radio buttons** to simulate acceptance/refusal of application shutdown.  
  - Displays transmitted parameters to demonstrate **start/stop messaging**.  


### 🔄 Upgrade Notes  
1. **Ensure all instances (Master, Agent, Client) are updated** to **v2.0.0**.  
2. **Update integrations** that rely on `TAppWatcherMessage` due to **structure changes**.  
3. **Reconfigure deployment settings** via the new `Deploy Manager` UI.  

🚀 **With v2.0.0, AppWatcher is now a full-fledged **deployment tool** in addition to monitoring!** 🎉  

---

## v1.3.4 - 28/02/2025  
### ✨ Added  
- Added a **filtering system** in `TStringGrid`, allowing users to filter applications dynamically.  

---

## v1.3.3 - 27/02/2025  
### 🚀 Improved  
- Improved **clean shutdown process** for the Indy server when closing the application.  

---

## v1.3.2 - 25/02/2025  
### 🛠 Fixed  
- **Application shutdown logic**: The shutdown request now correctly applies **only** to the targeted application, instead of affecting all running instances.  
- **Ensured correct working directory**: Restarted applications now launch from their **original** working directory instead of the system default.  

---

## v1.3.0 - 24/02/2025  
### 🚀 Improved  
- **Sorting in application list**: Fixed an issue where column sorting in the Master did not support all columns correctly.  
  - Now supports sorting by **all six columns** (previously limited to five).  
- **UI Enhancement**: Added a **Splitter** between the application list and the message log, allowing users to adjust the interface for better readability.  

### 📖 Documentation  
- Updated `README.md` to clarify that **AppWatcher** is a Delphi component, specifically designed for **Delphi 12.2 Athens**.  

---

## v1.2.1 (Patch) - 25/02/2025  
### 🛠 Fixed  
- **Missing `Config\` folder check** in `FindConfigPath`.  
  - Ensures `.ini` files can be found even if placed inside `Config\`.  

---

## v1.2.0 - 23/02/2025  
### 🛠 Fixed  
- **INI Path Resolution**: Improved how configuration files are located, ensuring proper detection even with symbolic links or shortcuts.  
  - Now supports `.lnk` files, allowing better compatibility with Windows shortcuts.  

### 🔥 Cleanup  
- **Removed** the old setup script (`AppWatcherLink.cmd`), which is no longer needed.  

---

## v1.1.0 - 23/02/2025  
### 🚀 Improved  
- **Client Port Management**:  
  - The agent now correctly loads the **client port settings** from `AppWatcher.ini`.  
  - Ensures proper communication between client and agent.  
- **UI & Language Enhancements**:  
  - Translated **application titles**: Agent, Master, and Client now use localized titles.  
  - Titles are stored in language files:  
    - `AppWatcher_lang_fr.ini` (French)  
    - `AppWatcher_lang_en.ini` (English)  
  - **Improved wording** for better clarity.  

---

## v1.0 - 09/02/2025  
🎉 First **stable version** released!  
