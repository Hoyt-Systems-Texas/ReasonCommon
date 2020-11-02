open Jest
open Expect

type test_value = {
  key: int;
  parent: int option;
  value: string;
}

module Test_tree_lookup = struct 
  type value = test_value

  let get_key value = value.key

  let get_parent value = value.parent

end

module Test_tree = Tree.Make_tree(Test_tree_lookup)

let () =
  let value = [
    {
      key=1;
      parent=None;
      value="Me"
    };
    {
      key=2;
      parent=Some 1;
      value="Other"
    };
    {
      key=3;
      parent=Some 2;
      value="Other 3"
    };
    {
      key=4;
      parent=Some 2;
      value="Other 4"
    };
    {
      key=5;
      parent=Some 4;
      value="Other 5"
    };
    {
      key=6;
      parent=Some 4;
      value="Other 6"
    };
  ] in
  describe "Simple tree lookup." @@ fun _ -> 
    let tree_lookup = Test_tree.make value in
    let children = Test_tree.children tree_lookup 1 in
    test "Get children" (fun () ->
      let result = List.length children in
      expect result |> toBe 1);

    test "Get root node test" @@ (fun () ->
      let result = Test_tree.get tree_lookup 1 in
      let key = Core.OptionExt.map result @@ fun v -> v.key in
      expect key |> toBe @@ Some 1);

    test "Get second child test" @@ (fun () ->
      let result = Test_tree.children tree_lookup 2 in
      let total = List.length result in
      expect total |> toBe 2);

    test "Remove node test" @@ (fun () ->
      Test_tree.remove tree_lookup 4;
      let result = Test_tree.children tree_lookup 2 in
      let total = List.length result in
      expect total |> toBe 3)
