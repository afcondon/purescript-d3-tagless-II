// foreign import readJSONJS               :: String -> TreeJson -- TODO no error handling at all here RN
exports.readJSON_ = filecontents => JSON.parse(filecontents)
