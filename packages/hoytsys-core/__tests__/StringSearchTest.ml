open! Jest
open! Expect

type test_value = {
  id: int;
  words: string;
}

module Test_value_search_type = struct
  type t = test_value

  let get_text t = t.words

  let tokenize ~func ~acc s =
    let words = HoytsysCore.Core.Tokenizer.tokenizeString s in
    Belt.List.reduceWithIndex words acc func

end

module Test_value_search = HoytsysCore.StringSearch.Make_string_search(Test_value_search_type)

let () =
  let documents = [|
    {
      id=1;
      words="Hello world";
    };
    {
      id=2;
      words="Other text world";
    };
    {
      id=3;
      words="Something else";
    }
  |] in
  describe "String search test" @@ fun _ ->
    test "hello world!" (fun () ->
      let string_search = Test_value_search.make documents in
      expect "true" |> toBe "true")