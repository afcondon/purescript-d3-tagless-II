// foreign import readJSONJS               :: String -> TreeJson -- TODO no error handling at all here RN
export function readJSON_(filecontents) { return JSON.parse(filecontents); }
