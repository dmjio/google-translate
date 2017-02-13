{ mkDerivation, aeson, base, bytestring, http-api-data, http-client
, servant, servant-client, stdenv, text, transformers
}:
mkDerivation {
  pname = "google-translate";
  version = "0.3";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring http-api-data http-client servant
    servant-client text transformers
  ];
  description = "Google Translate API bindings";
  license = stdenv.lib.licenses.bsd3;
}
