# 🚀 AppWatcher – Remote Supervision & Deployment for Developers

## A tool to gracefully stop and restart remote applications, in order to unlock the executable on a shared drive and allow its replacement.

[Cette page en Français](https://github.com/mbaumsti/Delphi-App-Watcher/tree/main/LISEZ_MOI.md)

**AppWatcher** is a **Delphi component** that allows an application to be **remotely controlled** by a dedicated **supervisor application** included in the solution.

It is designed for **developers and IT administrators** who need to:

- **Gracefully stop** applications  
- **Deploy to a shared folder**  
- **Restart** them remotely  

All of this **without using RDP** or physically accessing each machine.

![AppWatcherMasterEN](https://github.com/user-attachments/assets/3a717d45-8f6c-44f7-a537-d49768c8574c)

---

## ⭐ Why Use AppWatcher?

✔️ **Easily stop running applications on remote machines**  
✔️ **Deploy application updates with minimal disruption to users**  
✔️ **Notify users before stopping an application**  
✔️ **Automatically copy apps from one source to specific destinations**  
✔️ **Restart applications automatically after an update**  
✔️ **Avoid using RDP (Remote Desktop) or physically moving between machines**  
✔️ **Minimize downtime and streamline the deployment process**  

---

## 🚀 Features

✅ **Remote application shutdown** with a simple command.  
✅ **Configurable countdown before shutdown** to notify users.  
✅ **Controlled application restart** after an update.  
✅ **Multilingual support** (French & English).  
✅ **Configurable settings** via INI files.  
✅ **Communication via Indy TCP/IP and Named Pipes** for secure messaging.  
✅ **Lightweight and efficient** – does not require admin privileges.  
✅ **New (v2.0): Application deployment** – automatic and optimized copying of executable files.  
✅ **New (v3.0): New architecture using Named Pipes** for local communication between Clients and the Agent.  
✅ **New (v3.0): New `AppWatcherStub` utility** to relaunch the Agent after it has been stopped.  
✅ **New (v3.1): Backup of replaced executables** with rotation and recovery management.

---
## 🧩 Requirements

- Delphi 10.2 Tokyo → Delphi 12 Athens
- Windows 32-bit or 64-bit
- [Raize Components](https://www.raize.com/DevTools/Download/index.htm) library (for `TRzNumericEdit`)  
  - ✅ **If you have Delphi 10.2+ Professional/Enterprise**, Raize Components may already be included.  
  - 🔔 **Otherwise**, install Raize Components manually or replace `TRzNumericEdit` with a standard `TSpinEdit` (requires minor UI adjustments).

- Indy Components (comes with Delphi by default)
- [NamedPipesForDelphi](https://github.com/superflexible/NamedPipesForDelphi) (already included inside AppWatcher sources)

--

## 🛠️ How It Works

AppWatcher consists of **three main components**:  

1. **🖥️ AppWatcher Master** – The **central server** that:  
   
   - Communicates with all **Agents** and keeps track of their presence.  
   - Lists all **applications managed by the Agents** (i.e., applications using the `TAppWatcherClient` component).  
   - Allows administrators to **remotely stop applications**, while giving users a countdown before shutdown.  
   - Supports **cancellation of a STOP request** before the countdown expires.  
   - Can request **all managed applications to restart** after an update.  
   - Can request all **Agents to shut down**. 
   - **New (v3.0): AppWatcherStub utility** to allow restarting agent after stopping it. 
   - **New (v2.0): Manages the list of applications to deploy** via a dedicated interface. 
   - **New (v2.0): Sorting and filtering** to display only the applications to deploy. 
   - **New (v3.1): Backup of replaced executables** with rotation and recovery management.

2. **🖥️ AppWatcher Agent** – A **lightweight background application** running on remote machines that:
   
   - Listens for **commands** from the Master.  
   - Communicates with **local applications** using the AppWatcher Client component.  
   - Notifies users and requests applications to **stop** when an update is needed.  
   - Maintains a **local list of applications** to restart after an update.  

3. **🖥️ AppWatcher Client Component** – A **Delphi component (`TAppWatcherClient`)** that:  
   
   - Allows an application to **communicate with the local Agent** with minimal programming effort.  
   - Handles **STOP requests** requested by the Master and transmitted by the Agent.  
   - Gives the developer control over whether to **accept or refuse** the STOP request, based on the application's state.  
   - Provides **command-line parameters** that the Agent will use when restarting the application.  
   - Ensures a **clean shutdown and possible restart after an update**.  
   - Simplifies integration of AppWatcher into Delphi applications with minimal coding.
   - 
---

## 🔥 Delphi Version Notes

**✅ This application was originally developed in Delphi v12.2.**

Depending on your Delphi version, please open the appropriate package:

| Delphi Version | Package to open |
|:---------------|:----------------|
| Delphi 10.2 Tokyo / 10.3 Rio | `AppWatcherClientPackage_Pre104.dpk` |
| Delphi 10.4 Sydney and later | `AppWatcherClientPackage.dpk` |

**Important:**  
- `{$LIBSUFFIX 'Auto'}` is supported only from Delphi 10.4 onward.
- Using the wrong package may cause build or linking errors.


🙏 **Special thanks** to [limelect](https://github.com/limelect) for his help in adapting AppWatcher to Delphi 10.2 compatibility.

---

## 📦 Installation

👉 **Precompiled binaries are available for quick testing without compilation.**
📌 [Download here](https://github.com/mbaumsti/Delphi-App-Watcher/releases/latest)


### 📚 Developer Setup Instructions

If you are building AppWatcher from source:

- All `.dproj` and `.groupproj` files have been removed to avoid Delphi version conflicts.
- You must manually open and compile the `.dpk` and `.dpr` files inside your version of Delphi.
- Please follow the detailed guide in **[BUILD_INSTRUCTIONS.md](BUILD_INSTRUCTIONS.md)**.

👉 It explains:
- How to open the right packages (`.dpk`)
- How to install the component
- How to configure Delphi's Library Paths correctly
- How to rebuild the missing `.dproj` or `.groupproj` files if needed.


### 🔹 **1. Setting Up the Master Server**

- Run `AppWatcherMaster.exe` on the machine that will act as the **control center**.  
- The **Master automatically updates its IP address** in the `AppWatcher.ini` file.  
- Agents **regularly read the INI file** to locate the active Master.  
- **No manual configuration** is needed unless you want to change the default port.  
- The **last Master started on the network takes control**.  

### 🔹 **2. Deploying the Agent on Remote Machines**

- Copy `AppWatcherAgent.exe` to all machines that need remote control.  
- Run the **Agent**, and it will appear as an **icon in the system tray** (notification area).  
- **Double-clicking the icon opens the log window**, which displays connection status and received commands.  
- **Right-clicking the icon opens a menu** that allows you to **exit the Agent**, but:  
  - To prevent users from stopping the Agent, **exiting is protected by a password**.  
  - The current password (not secure) is **`appW`**.  
- **Closing the log window using the "X" button does not stop the Agent** – it simply hides the window.  
- To **fully exit the Agent**, hold **SHIFT + CONTROL** while clicking the "X" button to display the password prompt.  
- The Agent **automatically reads the INI file** to locate the active Master.  

### 🔹 **3. Integrating the Client Component in Your Delphi Application**

To make a **Delphi application controllable** by AppWatcher, follow these steps:  

1. **Install the `TAppWatcherClient` Component**:  
   
   - Open `AppWatcherClientPackage.dproj` in Delphi.  
   - Compile and install the package.  
   - Add the component's source path to **Delphi's library paths** (Tools → Options → Library → Library Path).  

2. **Add `TAppWatcherClient` to Your Applications**:  
   
   - Place a `TAppWatcherClient` component on main form in all applications you want to manage.  
   - The component allows the application to communicate with the local Agent and respond to STOP commands.  

3. **Handle STOP Requests** (Prevent shutdown if needed):  
   
   - Implement the `OnStopRequested` event to **prevent the application from closing** if, for example, it has unsaved data.  
   - Example:  
   
   ```delphi
   procedure TFormMain.AppWatcherClient1StopRequested(Sender: TObject; var CanStop: Boolean);
   begin
     if UnsavedChanges then
       CanStop := False // Prevent shutdown if data is not saved
     else
       CanStop := True;  // Allow shutdown
   end; 
   ```

4. **Define Restart Parameters** (Command-line arguments for relaunching the app):
   
   - Use the OnGetAppParams event to send command-line parameters that the Agent should use when restarting the application.
   - Example:  
   
   ```delphi
    procedure TFormMain.AppWatcherClient1GetAppParams(Sender: TObject; var Params: string);
    begin
        Params:='';
        if TestMode then
            Params := '/Mode=test';  // Example parameter
    end;   
   ```
   
---

### 🔹 **4. Configuration file management (`.ini and .json`)**

AppWatcher **uses INI files** for its configuration and an **AppWatcher.json** file (to store the list of applications to deploy). These files must be **accessible by the application** to ensure proper operation.

📌 Where does AppWatcher look for files?  
 
✔ **In the application's runtime directory** (e.g. `C:\Program Files\AppWatcher\`).   
✔ **In a `Config\` subdirectory of the runtime directory** (e.g. `C:\Program Files\AppWatcher\Config\`).  
✔ **By following Windows shortcuts (`.lnk`)**: If a `.ini` file is not found directly, AppWatcher checks if a `.lnk` shortcut with the same name exists and follows its target.

💡 If **`AppWatcher.ini` is missing**, the application will display an error message. 

---

### 🔹 **5. Test Application `AppWatcherClient.dproj`**

The `AppWatcherClient.dproj` application is provided to **test the integration** of the `TAppWatcherClient` component **without modifying your own application**.  

- It includes a `TAppWatcherClient` component configured to **receive and display commands** sent by the Agent.  
- When a **STOP** command is received, the application can **accept or refuse shutdown** via the `OnStopRequested` event.  
- It also allows you to **simulate sending restart parameters** using the `OnGetAppParams` event.  
- All received actions and messages are displayed in a **log window (`Memo1`)** for monitoring and analysis.  
- The test application also includes a **second form** (opened via the "Second Form" button) which **also contains a `TAppWatcherClient` component**.  
  - This demonstrates that only the **component on the MainForm** handles STOP requests.  
  - A `TAppWatcherClient` placed on a secondary form **will not work**, as the **main application window takes priority**.  
  - This allows for flexible UI design where a **window may act as the MainForm in one application but as a secondary form in another**.  

This tool allows you to **test AppWatcher’s functionality** before integrating `TAppWatcherClient` into your final applications. 🚀 

---

## 🔧 External Resources

🖼 Icons Attribution:

Some icons used in this project are from [Icons8](https://icons8.com).
As per Icons8's licensing, attribution is required unless you have a paid subscription.

🔌 Named Pipes Library:
This project integrates the excellent [NamedPipesForDelphi](https://github.com/superflexible/NamedPipesForDelphi) library, originally created by Russell and published by Tobias Giesen under *The Unlicense*.  
The source has been modularized into three units (`PipesCommon`, `PipeClient`, `PipeServer`) for better integration into AppWatcher.

---

## Changelog

 The full version history is available in [Changelog.md](CHANGELOG.md).
