{ sources ? import ./sources.nix }:
[(self: super: {
  haskell-nix = (import sources."haskell.nix" { pkgs = self; hackageSourceJSON = ./hackage-src.json; });


  /*
  * Run a series of commands only for their exit status, producing an empty
  * closure.
  */
  runCheck = script: src:  self.runCommand "check" {} ''
    src="${src}"
    ${script}
    touch $out
  '';

  /*
  * Check the given target path for files with trailing whitespace, fail if any
  * are found
  */
  checkTrailingWhitespace = self.runCheck ''
    files=$(grep --recursive --files-with-matches --binary-files=without-match '[[:blank:]]$' "$src" || true)
    if [[ ! -z $files ]];then
      echo '  Files with trailing whitespace found:'
      for f in "''${files[@]}"; do
        echo "  * $f" | sed -re "s|$src/||"
      done
      exit 1
    fi
  '';
})]
