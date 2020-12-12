# __PROJNAME__

This project is Lazarus, Castle Editor and build-tool friendly

##### To compile with Lazarus (the only option for MacOS)

Load __PROJNAME__Laz.lpi into Lazarus + compile

##### To compile with Castle-Editor

Open CastleEngineManifest.xml in Castle Editor and select Compile And Run from the Run menu item

##### To compile from the command line for Windows/Linux

castle-engine compile

./__PROJNAME__App

##### To compile from the command line for Android

castle-engine package  --target=android

Install __PROJNAME__App-debug.apk / __PROJNAME__App-release.apk on a device

##### To compile from the command line for iOS

castle-engine package  --target=ios

Install on a device or use simulator

