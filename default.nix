{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
, projectOverrides ? {}
}:
with obelisk;
project ./. ({ hackGet, pkgs, ... }:
  let
    beamSrc = hackGet dep/beam;
    gargoyleSrc = hackGet dep/gargoyle;
  in {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  packages = {
    beam-core = beamSrc + /beam-core;
    beam-postgres = beamSrc + /beam-postgres;
    beam-migrate = beamSrc + /beam-migrate;
    mmark = hackGet dep/mmark;
    megaparsec = hackGet dep/megaparsec;
  };

  overrides = self: super: with pkgs.haskell.lib; {
    beam-core = dontCheck super.beam-core;
    beam-postgres = dontCheck super.beam-postgres;  # Requires PG to run tests
    email-validate = dontCheck super.email-validate;
    versions = self.callHackage "versions" "3.5.1" {};
    temporary = dontCheck super.temporary;
    unliftio = dontCheck super.unliftio;
    neat-interpolation = doJailbreak super.neat-interpolation;
    mmark = dontCheck (dontHaddock super.mmark);
    modern-uri = dontCheck (doJailbreak super.modern-uri);
    mono-traversable = dontCheck super.mono-traversable;
    conduit = dontCheck super.conduit;
    yaml = dontCheck super.yaml;
  } // import gargoyleSrc self;
} // projectOverrides)
