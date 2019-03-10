workflow "Build and test on push" {
  on = "push"
  resolves = ["Compile"]
}

action "Compile" {
  uses = "./.github/image"
  runs = "stack"
  args = "build"
}

action "Test" {
  uses = "./.github/image"
  runs = "stack"
  args = "test"
}
