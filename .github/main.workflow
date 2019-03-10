workflow "Build and test on push" {
  on = "push"
  resolves = ["Compile"]
}

action "Compile" {
  uses = "docker://library/haskell:latest"
  runs = "ls"
  args = "/usr/local/bin"
}
