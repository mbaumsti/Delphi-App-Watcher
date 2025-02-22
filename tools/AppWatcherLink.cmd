@echo off
chcp 1252 >nul
setlocal EnableDelayedExpansion

:: ?? Détection automatique de la langue du système
for /f "tokens=3" %%L in ('REG QUERY "HKEY_CURRENT_USER\Control Panel\International" /v "LocaleName" 2^>nul') do set "LOCALE=%%L"
echo %LOCALE%
set "LOCALE=%LOCALE:~0,2%"  :: Récupère uniquement les deux premières lettres

:: Déterminer la langue : Français si "fr", sinon Anglais
set "LANG=en"
if "%LOCALE%"=="fr" set "LANG=fr"
if /I "%1"=="fr" set "LANG=fr"
if /I "%1"=="en" set "LANG=en"

:: Messages en fonction de la langue
if "%LANG%"=="fr" (
    set "MSG_TITLE=AppWatcher - Configuration de l'installation"
    set "MSG_SOURCE=Entrez le chemin du dossier source contenant les fichiers .ini (ex: C:\AppWatcher\config): "
    set "MSG_ERROR_SOURCE=Erreur : Le dossier source est introuvable ou inaccessible."
    set "MSG_DEST=Entrez le dossier où créer les liens symboliques (ex: C:\Program Files\AppWatcher): "
    set "MSG_CREATING=Le dossier destination n'existe pas, création en cours..."
    set "MSG_ERROR_DEST=Erreur : Impossible de créer le dossier destination."
    set "MSG_ADMIN=Ce script doit être exécuté en mode administrateur."
    set "MSG_ERROR_ACCESS=Erreur : Impossible d'accéder au dossier destination."
    set "MSG_LINKS=Création des liens symboliques..."
    set "MSG_SUCCESS=? Liens symboliques créés avec succès !"
    set "MSG_ERROR_LINK=? Erreur lors de la création des liens symboliques."
) else (
    set "MSG_TITLE=AppWatcher - Setup Configuration"
    set "MSG_SOURCE=Enter the source folder path containing .ini files (ex: C:\AppWatcher\config): "
    set "MSG_ERROR_SOURCE=Error: Source folder not found or inaccessible."
    set "MSG_DEST=Enter the destination folder to create symbolic links (ex: C:\Program Files\AppWatcher): "
    set "MSG_CREATING=Destination folder does not exist, creating..."
    set "MSG_ERROR_DEST=Error: Failed to create the destination folder."
    set "MSG_ADMIN=This script must be run as administrator."
    set "MSG_ERROR_ACCESS=Error: Unable to access the destination folder."
    set "MSG_LINKS=Creating symbolic links..."
    set "MSG_SUCCESS=? Symbolic links successfully created!"
    set "MSG_ERROR_LINK=? Error while creating symbolic links."
)

echo.
echo ============================================
echo       %MSG_TITLE%
echo ============================================
echo.

:: Demander le dossier source
set /p SOURCE_DIR="%MSG_SOURCE%"
if not exist "%SOURCE_DIR%" (
    echo %MSG_ERROR_SOURCE%
    pause
    exit /b 1
)

:: Demander le dossier de destination
set /p DEST_DIR="%MSG_DEST%"
if not exist "%DEST_DIR%" (
    echo %MSG_CREATING%
    mkdir "%DEST_DIR%" || (
        echo %MSG_ERROR_DEST%
        pause
        exit /b 1
    )
)

:: Vérifier les droits administrateurs
net session >nul 2>&1
if %errorLevel% neq 0 (
    echo %MSG_ADMIN%
    pause
    exit /b 1
)

:: Aller dans le dossier de destination
pushd "%DEST_DIR%" >nul 2>&1 || (
    echo %MSG_ERROR_ACCESS%
    pause
    exit /b 1
)

:: Supprimer les liens existants
if exist "AppWatcher_lang_fr.ini" del "AppWatcher_lang_fr.ini"
if exist "AppWatcher_lang_en.ini" del "AppWatcher_lang_en.ini"
if exist "AppWatcher.ini" del "AppWatcher.ini"

:: Créer les liens symboliques
echo.
echo %MSG_LINKS%
mklink "AppWatcher_lang_fr.ini" "%SOURCE_DIR%\AppWatcher_lang_fr.ini"
if %errorLevel% neq 0 goto :error
mklink "AppWatcher_lang_en.ini" "%SOURCE_DIR%\AppWatcher_lang_en.ini"
if %errorLevel% neq 0 goto :error
mklink "AppWatcher.ini" "%SOURCE_DIR%\AppWatcher.ini"
if %errorLevel% neq 0 goto :error

:: Revenir au dossier précédent
popd

echo.
echo %MSG_SUCCESS%
pause
exit /b 0

:error
echo %MSG_ERROR_LINK%
popd
pause
exit /b 1
