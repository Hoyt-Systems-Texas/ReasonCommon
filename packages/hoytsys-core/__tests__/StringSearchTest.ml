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
      words="Something me else";
    }
  |] in
  describe "String search test" @@ fun _ ->
    test "hello world!" (fun () ->
      let string_search = Test_value_search.make documents in
      let result = Test_value_search.search "hello" string_search in
      match Belt.Array.get result 0 with
      | Some doc -> expect doc.id |> toBe 1;
      | None -> failwith "Document didn't match";
    );

    test "He" (fun () ->
      let string_search = Test_value_search.make documents in
      let result = Test_value_search.search "he" string_search in
      expect (Belt.Array.length result) |> toBe 1);

    test "me" (fun () ->
      let string_search = Test_value_search.make documents in
      let result = Test_value_search.search "me" string_search in
      expect (Belt.Array.length result) |> toBe 1);

    test "other" (fun () ->
      let string_search = Test_value_search.make documents in
      let result = Test_value_search.search "other" string_search in
      expect (Belt.Array.length result) |> toBe 1);

    test "oth" (fun () ->
      let string_search = Test_value_search.make documents in
      let result = Test_value_search.search "oth" string_search in
      expect (Belt.Array.length result) |> toBe 1);

    test "text" (fun () ->
      let string_search = Test_value_search.make documents in
      let result = Test_value_search.search "text" string_search in
      expect (Belt.Array.length result) |> toBe 1);

    test "te" (fun () ->
      let string_search = Test_value_search.make documents in
      let result = Test_value_search.search "te" string_search in
      expect (Belt.Array.length result) |> toBe 1);

    test "Something" (fun () ->
      let string_search = Test_value_search.make documents in
      let result = Test_value_search.search "something" string_search in
      expect (Belt.Array.length result) |> toBe 1);

    test "Som" (fun () ->
      let string_search = Test_value_search.make documents in
      let result = Test_value_search.search "som" string_search in
      expect (Belt.Array.length result) |> toBe 1);

    test "Something" (fun () ->
      let string_search = Test_value_search.make documents in
      let result = Test_value_search.search "some" string_search in
      expect (Belt.Array.length result) |> toBe 1);

    test "World" (fun () ->
      let string_search = Test_value_search.make documents in
      let result = Test_value_search.search "world" string_search in
      expect (Belt.Array.length result) |> toBe 2);

    test "World" (fun () ->
      let string_search = Test_value_search.make documents in
      let result = Test_value_search.search "wor" string_search in
      expect (Belt.Array.length result) |> toBe 2);

