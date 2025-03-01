# 🚀 AppWatcher - Remote Application Management for Developers

**AppWatcher** is a **Delphi component** that allows an application to be **remotely controlled** by a dedicated **supervisor** application, included in the solution.  

It is designed for **developers and IT administrators** who need to **gracefully stop**,  
**replace**, and **restart applications** across multiple machines, without using **RDP** or manually accessing each computer.

**✅ Developed in Delphi 12.2**  

## ⭐ Why Use AppWatcher?

✔️ **Easily stop running applications on remote machines**  
✔️ **Deploy application updates with minimal disruption to users**  
✔️ **Notify users before stopping an application**  
✔️ **Restart applications automatically after an update**  
✔️ **Avoid using RDP (Remote Desktop) or physically moving between machines**  
✔️ **Minimize downtime and streamline the deployment process**  

## 🚀 Features

✅ **Stop applications remotely** with a simple command.  
✅ **Define a countdown before stopping an application** to notify users.  
✅ **Restart applications automatically** after an update.  
✅ **Multi-language support** (French & English).  
✅ **Configurable settings** via INI files.  
✅ **Indy TCP/IP communication** for secure messaging.  
✅ **Lightweight and efficient**—does not require admin privileges.   

## 🛠️ How It Works

AppWatcher consists of **three main components**:  

1. **🖥️ AppWatcher Master** – The **central server** that:  
   
   - Communicates with all **Agents** and keeps track of their presence.  
   - Lists all **applications managed by the Agents** (i.e., applications using the `TAppWatcherClient` component).  
   - Allows administrators to **remotely stop applications**, while giving users a countdown before shutdown.  
   - Supports **cancellation of a STOP request** before the countdown expires.  
   - Can request **all managed applications to restart** after an update.  
   - Can request all **Agents to shut down**.  

2. **🖥️ AppWatcher Agent** – A **lightweight service** running on remote machines that:  
   
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

## 📦 Installation

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

### 🔹 **4. Managing Configuration Files (`.ini`)**

AppWatcher **relies on INI files** for configuration. These files must be **accessible to the application** to ensure proper operation.  

📌 Where does AppWatcher look for INI files?
✔ In the application's execution path (e.g., C:\Program Files\AppWatcher\).
✔ Inside a Config\ subdirectory of the execution path (e.g., C:\Program Files\AppWatcher\Config\).
✔ Following Windows shortcuts (.lnk): If an .ini file is not found directly, AppWatcher checks if a shortcut (.lnk) with the same name exists and follows its target.

💡 If **`AppWatcher.ini` is missing**, the application  display an error message.  

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

## Assets

🖼 Icons Attribution:

Some icons used in this project are from [Icons8](https://icons8.com).
As per Icons8's licensing, attribution is required unless you have a paid subscription.

## Changelog

 The full version history is available in [Changelog.md](CHANGELOG.md).
