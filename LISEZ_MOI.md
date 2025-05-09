# 🚀 AppWatcher – Supervision & Déploiement à Distance pour les Développeurs  

## Outil permettant de stopper proprement puis redémarrer des applications distantes, afin de libérer l’exécutable sur un disque partagé pour le remplacer.

[This page in English](https://github.com/mbaumsti/Delphi-App-Watcher/tree/main/README.md) 

**AppWatcher** est un **composant Delphi** permettant à une application d’être **contrôlée à distance** par une **application superviseur dédiée**, incluse dans la solution.

Il est conçu pour les **développeurs et administrateurs IT** qui doivent :

- **Arrêter proprement** des applications  
- **Déployer sur un dossier partagé**  
- **Les redémarrer à distance**  

Tout cela **sans utiliser RDP** ni accéder physiquement à chaque machine.


![AppWatcherMaster](https://github.com/user-attachments/assets/30e6fae7-94f2-4479-9a96-fe347d16bf9c)

---

## ⭐ Pourquoi utiliser AppWatcher ?  

✔️ **Arrêtez facilement des applications en cours d'exécution sur des machines distantes**  
✔️ **Déployez des mises à jour d'application avec un minimum de perturbations pour les utilisateurs**  
✔️ **Prévenez les utilisateurs avant l'arrêt d'une application**  
✔️ **Copiez automatiquement des applications depuis une source vers des destinations spécifiques**  
✔️ **Redémarrez automatiquement les applications après une mise à jour**  
✔️ **Évitez d'utiliser RDP (Remote Desktop) ou de vous déplacer physiquement entre les machines**  
✔️ **Minimisez les temps d'arrêt et optimisez le processus de déploiement**  

---

## 🚀 Fonctionnalités  

 
✅ **Arrêt des applications à distance** avec une simple commande.  
✅ **Définition d’un compte à rebours avant l'arrêt** pour notifier les utilisateurs.  
✅ **Redémarrage controlé des applications** après une mise à jour.  
✅ **Support multilingue** (Français & Anglais).  
✅ **Paramètres configurables** via des fichiers INI.  
✅ **Communication via Indy TCP/IP et Named pipes** pour une messagerie sécurisée.  
✅ **Léger et efficace** – ne nécessite pas de privilèges administrateur.  
✅ **Nouveau (v2.0): Déploiement d'applications** : Copie automatique et optimisée des fichiers exécutables.  
✅ **Nouveau (v3.0): Nouvelle architecture utilisant les Named Pipes** pour la communication locale entre les Clients et l'Agent.  
✅ **Nouveau (v3.0) : Nouvel utilitaire `AppWatcherStub`** permettant de relancer l'Agent après l'avoir arrêté.  
✅ **Nouveau (v3.1) : Sauvegarde des executables remplacés** avec gestion des rotations et de la récupération.  

---

## 🧩 Prérequis

- Delphi 10.2 Tokyo → Delphi 12 Athens
- Windows 32 bits ou 64 bits
- [Raize Components](https://www.raize.com/DevTools/Download/index.htm) (nécessaire pour le composant `TRzNumericEdit`)  
  - ✅ **Si vous disposez de Delphi 10.2+ Professionnel ou Enterprise**, Raize Components est peut-être déjà installé.  
  - 🔔 **Sinon**, vous devrez installer Raize Components manuellement ou remplacer `TRzNumericEdit` par un `TSpinEdit` standard (ce qui nécessitera quelques ajustements mineurs sur l'interface).

- Indy Components (déjà inclus avec Delphi)
- [NamedPipesForDelphi](https://github.com/superflexible/NamedPipesForDelphi) (déjà intégré aux sources de AppWatcher)

--

## 🛠️ Comment ça fonctionne ?  

AppWatcher est composé de **trois éléments principaux** :  

1. **🖥️ AppWatcher Master** – Le **serveur central** qui :  

- Communique avec tous les **Agents** et garde une trace de leur présence.  
- Liste toutes les **applications gérées par les Agents** (c'est-à-dire les applications utilisant le composant `TAppWatcherClient`).  
- Permet aux administrateurs **d'arrêter à distance des applications**, tout en affichant un compte à rebours aux utilisateurs avant l'arrêt.  
- Permet **d'annuler une demande d'arrêt** avant la fin du compte à rebours.  
- Peut demander **le redémarrage de toutes les applications gérées** après une mise à jour.  
- Peut demander **l’arrêt de tous les Agents**.
- **Nouveau (v2.0) : Gère la liste des applications à déployer** via une interface dédiée.   
- **Nouveau (v2.0) : Tri et filtrage** pour afficher uniquement les applications à déployer.  
- **Nouveau (v3.0): Utilitaire AppWatcherStub** qui permet de relancer l'agent après l'avoir arrêté.  
- **Nouveau (v3.1) : Sauvegarde des executables remplacés** avec gestion des rotations et de la récupération.  

2. **🖥️ AppWatcher Agent** – Une **application légère** exécutée en arrière plan sur les machines distantes qui :

- Écoute les **commandes** envoyées par le Master.  
- Communique avec les **applications locales** avec des Named pipes via le composant `TAppWatcherClient`.  
- Notifie les utilisateurs et demande aux applications de **s'arrêter** lorsqu'une mise à jour est nécessaire.  
- Maintient une **liste locale des applications** à redémarrer après la mise à jour.


3. **🖥️ Composant Client AppWatcher** – Un **composant Delphi (`TAppWatcherClient`)** qui :  

- Permet à une application de **communiquer avec l'Agent local** avec un effort de programmation minimal.  
- Gère les **demandes d'arrêt** demandées par le Master et transmises par l'Agent.  
- Donne au développeur la possibilité **d'accepter ou de refuser** la demande d'arrêt, en fonction de l’état de l’application.  
- Fournit des **paramètres de ligne de commande** que l’Agent utilisera lors du redémarrage de l’application.  
- Assure un **arrêt propre et un redémarrage possible après mise à jour**.  
- Facilite l’intégration d’AppWatcher dans des applications Delphi avec **un minimum de code**.  

 
---

## 🔥 Notes de version Delphi

**✅ Cette application a été initialement développée en Delphi v12.2.**

Selon votre version de Delphi, veuillez ouvrir le paquet approprié:

| Version Delphi | Paquet à ouvrir |
|:---------------|:----------------|
| Delphi 10.2 Tokyo / 10.3 Rio | `AppWatcherClientPackage_Pre104.dpk` |
| Delphi 10.4 Sydney et versions ultérieures | `AppWatcherClientPackage.dpk` |

**Important:**
- `{$LIBSUFFIX 'Auto'}` n'est pris en charge qu'à partir de Delphi 10.4.
- L'utilisation d'un paquet incorrect peut entraîner des erreurs de compilation ou de liaison.

🙏 **Remerciements spéciaux** à [limelect](https://github.com/limelect) pour son aide à l'adaptation d'AppWatcher à la compatibilité Delphi 10.2.

---

## 📦 Installation  

👉 **Des binaires précompilés sont disponibles pour un test rapide sans compilation.**  
📌 [Télécharger ici](https://github.com/mbaumsti/Delphi-App-Watcher/releases/latest)


### 📚 Instructions pour les développeurs

Si vous souhaitez **recompiler AppWatcher depuis les sources** :

- Tous les fichiers `.dproj` et `.groupproj` ont été supprimés pour éviter les conflits entre les différentes versions de Delphi.
- Vous devez **ouvrir et compiler manuellement** les fichiers `.dpk` et `.dpr` correspondant à votre version de Delphi.
- Merci de suivre le guide détaillé disponible dans **[BUILD_INSTRUCTIONS.md](BUILD_INSTRUCTIONS.md)**.

👉 Ce guide explique notamment :
- Comment ouvrir correctement les packages (`.dpk`)  
- Comment installer le composant dans Delphi  
- Comment configurer correctement les **chemins de bibliothèques** (`Library Paths`)  
- Comment reconstruire les fichiers `.dproj` ou `.groupproj` si nécessaire.  


### 🔹 **1. Configuration du serveur Master**  

- Exécutez `AppWatcherMaster.exe` sur la machine qui servira de **centre de contrôle**.  
- Le **Master met automatiquement à jour son adresse IP** dans le fichier `AppWatcher.ini`.  
- Les Agents **lisent régulièrement le fichier INI** pour localiser le Master actif.  
- **Aucune configuration manuelle** n’est nécessaire, sauf si vous souhaitez modifier le port par défaut.  
- **Le dernier Master démarré sur le réseau prend le contrôle**.  


### 🔹 **2. Déploiement de l’Agent sur les machines distantes**  

- Copiez `AppWatcherAgent.exe` sur toutes les machines nécessitant un contrôle à distance.  
- Exécutez l’**Agent**, qui apparaîtra sous forme d’**icône dans la barre de notification**.  
- **Double-cliquer sur l’icône ouvre la fenêtre de log**, affichant l’état de connexion et les commandes reçues.  
- **Un clic droit sur l’icône ouvre un menu** permettant de **quitter l’Agent**, mais :  
  - Pour empêcher les utilisateurs d’arrêter l’Agent, **la sortie est protégée par un mot de passe**.  
  - Le mot de passe actuel (non sécurisé) est **`appW`**.  
- **Fermer la fenêtre de log avec le bouton "X" ne stoppe pas l’Agent** – cela ne fait que masquer la fenêtre.  
- Pour **quitter totalement l’Agent**, maintenez **SHIFT + CONTROL** en cliquant sur le bouton "X" pour afficher l’invite de mot de passe.  
- L’Agent **lit automatiquement le fichier INI** pour localiser le Master actif.  


### 🔹 **3. Intégration du composant client dans votre application Delphi**  

Pour rendre une **application Delphi contrôlable** par AppWatcher, suivez ces étapes :  

1. **Installer le composant `TAppWatcherClient`** :  
   - Ouvrez `AppWatcherClientPackage.dproj` dans Delphi.  
   - Compilez et installez le package.  
   - Ajoutez le chemin source du composant à **Delphi** (Outils → Options → Bibliothèque → Chemin de la bibliothèque).  

2. **Ajouter `TAppWatcherClient` à vos applications** :  
   - Placez un `TAppWatcherClient` sur le **formulaire principal** des applications que vous souhaitez gérer.  

3. **Gérer les demandes d’arrêt** (Empêcher la fermeture si nécessaire) :  
   ```delphi
   procedure TFormMain.AppWatcherClient1StopRequested(Sender: TObject; var CanStop: Boolean);
   begin
     if UnsavedChanges then
       CanStop := False // Empêcher l'arrêt si des données ne sont pas sauvegardées
     else
       CanStop := True;  // Autoriser l'arrêt
   end; 
   ```

4. **Définir les paramètres de redémarrage** (Arguments de ligne de commande) :  
   ```delphi
   procedure TFormMain.AppWatcherClient1GetAppParams(Sender: TObject; var Params: string);
   begin
       Params := '';
       if TestMode then
           Params := '/Mode=test';  // Exemple de paramètre
   end;
   ```  

---

### 🔹 **4. Gestion des fichiers de configuration (`.ini et .json`)**  

AppWatcher **utilise des fichiers INI** pour sa configuration et un fichier **AppWatcher.json** (pour stocker la liste des applications a déployer). Ces fichiers doivent être **accessibles par l'application** afin d'assurer un fonctionnement correct.  

📌 Où AppWatcher cherche-t-il les fichiers ?  
✔ **Dans le répertoire d'exécution de l'application** (ex: `C:\Program Files\AppWatcher\`).  
✔ **Dans un sous-répertoire `Config\` du répertoire d'exécution** (ex: `C:\Program Files\AppWatcher\Config\`).  
✔ **En suivant les raccourcis Windows (`.lnk`)** : Si un fichier `.ini` n'est pas trouvé directement, AppWatcher vérifie si un raccourci `.lnk` portant le même nom existe et suit sa cible.  

💡 Si **`AppWatcher.ini` est manquant**, l'application affichera un message d'erreur.  

---

### 🔹 **5. Application de test `AppWatcherClient.dproj`**  

L’application **`AppWatcherClient.dproj`** est fournie pour **tester l’intégration** du composant `TAppWatcherClient` **sans modifier votre propre application**.  

- Elle inclut un composant `TAppWatcherClient` configuré pour **recevoir et afficher les commandes** envoyées par l’Agent.  
- Lorsqu'une commande **STOP** est reçue, l'application peut **accepter ou refuser l'arrêt** via l’événement `OnStopRequested`.  
- Elle permet également de **simuler l'envoi de paramètres de redémarrage** via l’événement `OnGetAppParams`.  
- Toutes les actions et messages reçus sont affichés dans une **fenêtre de log (`Memo1`)** pour surveillance et analyse.  
- L'application de test inclut aussi une **deuxième fenêtre** (ouverte via le bouton "Second Form") qui **contient également un composant `TAppWatcherClient`**.  
  - Cela démontre que seul le **composant présent sur le MainForm** gère les demandes d'arrêt.  
  - Un `TAppWatcherClient` placé sur un formulaire secondaire **ne fonctionnera pas**, car la **fenêtre principale de l'application prend la priorité**.  
  - Cela permet une conception flexible où une **fenêtre peut être le MainForm dans une application, mais un formulaire secondaire dans une autre**.  

Cet outil vous permet de **tester les fonctionnalités d’AppWatcher** avant d'intégrer `TAppWatcherClient` dans vos applications finales. 🚀  


---

## Ressources

🖼 Attribution des icônes :
Certaines icônes utilisées dans ce projet proviennent de [Icons8](https://icons8.com).  
Conformément à la licence de Icons8, une attribution est requise sauf si vous disposez d’un abonnement payant.

🔌 Bibliothèque Named Pipes :
Ce projet intègre l’excellente bibliothèque [NamedPipesForDelphi](https://github.com/superflexible/NamedPipesForDelphi), initialement développée par **Russell** et publiée par **Tobias Giesen** sous la licence *The Unlicense*.  
Le code source a été **modularisé en trois unités** (`PipesCommon`, `PipeClient`, `PipeServer`) afin de faciliter son intégration dans AppWatcher

---

## 📜 Historique des versions  

L'historique complet des versions est disponible dans [Changelog.md](CHANGELOG.md).  
