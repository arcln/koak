workflow "Build and test on push" {
  on = "push"
  resolves = ["Compile"]
}

action "Compile" {
  uses = "docker://library/haskell:latest"
  runs = "stack"
  args = "build"
}
