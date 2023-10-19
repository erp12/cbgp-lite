# Intended to be used in Docker containers to confirm all deps are installed correctly.
echo $(pyenv --version)
echo $(python --version)
echo $(java --version)
echo $(clojure --version)