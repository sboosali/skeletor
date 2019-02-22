##################################################
{}:

##################################################
let

config = {

  verbose = 2;
  jobs    = 4;

  nix           = false;
  deterministic = true;
  relocatable   = true;

  documentation = false;
  tests         = false;
  benchmarks    = false;
  coverage      = false;

  allow-older = false;
  allow-newer = false;

  username         = "sboo";
  password-command = "sboo-get-hackage-password";
                    # ^ defined in « ~/.profile ».

  #TODO ''${pass} "www.hackage.com/user/sboo"''

  remote-build-reporting = "detailed";

};

in
##################################################
let

bool = b:
  if b then "True" else "False";

int = builtins.toString;

path = builtins.toString;

in
##################################################
{



}
##################################################