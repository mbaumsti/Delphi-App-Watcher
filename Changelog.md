# Changelog  

## v1.3.4 - 28/02/2025  
### ✨ Added  
- Added a **filtering system** in `TStringGrid`, allowing users to filter applications dynamically.  

## v1.3.3 - 27/02/2025  
### 🚀 Improved  
- Improved **clean shutdown process** for the Indy server when closing the application.  

## v1.3.2 - 25/02/2025  
### 🛠 Fixed  
- **Application shutdown logic**: The shutdown request now correctly applies **only** to the targeted application, instead of affecting all running instances.  
- **Ensured correct working directory**: Restarted applications now launch from their **original** working directory instead of the system default.  

## v1.3.0 - 24/02/2025  
### 🚀 Improved  
- **Sorting in application list**: Fixed an issue where column sorting in the Master did not support all columns correctly.  
  - Now supports sorting by **all six columns** (previously limited to five).  
- **UI Enhancement**: Added a **Splitter** between the application list and the message log, allowing users to adjust the interface for better readability.  

### 📖 Documentation  
- Updated `README.md` to clarify that **AppWatcher** is a Delphi component, specifically designed for **Delphi 12.2 Athens**.  

## v1.2.1 (Patch) - 25/02/2025  
### 🛠 Fixed  
- **Missing `Config\` folder check** in `FindConfigPath`.  
  - Ensures `.ini` files can be found even if placed inside `Config\`.  

## v1.2.0 - 23/02/2025  
### 🛠 Fixed  
- **INI Path Resolution**: Improved how configuration files are located, ensuring proper detection even with symbolic links or shortcuts.  
  - Now supports `.lnk` files, allowing better compatibility with Windows shortcuts.  

### 🔥 Cleanup  
- **Removed** the old setup script (`AppWatcherLink.cmd`), which is no longer needed.  

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

## v1.0 - 09/02/2025  
🎉 First **stable version** released!  
