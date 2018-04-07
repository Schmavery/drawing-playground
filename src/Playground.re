type resultT('a, 'b) = Ok('a) | Error('b);

type rowT = {
  width: int,
  height: int,
  color: string
};

type svgRectT = {x: int, y: int, w: int, h: int, c: string};

type state = {
  textList: list(string),
  error: option(string),
  output: option(array(svgRectT)),
  data: array(rowT),
};

type action =
  | UpdateAndCompile(int, string)
  | DeleteTextBox(int);

let filteri = (f, lst) => {
  let (_, lst) = List.fold_left(((i, l), v) => {
        if (f(i, v)) {
          (i + 1, [v, ...l])
        } else {
          (i + 1, l)
        }
      }, (0, []), lst);
  lst
};

let component = ReasonReact.reducerComponent("Page");

let handleText = (codeNum, event, self) => {
  let target = ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event));
  let content: string = target##value;
  if (content == "") {
    self.ReasonReact.send(DeleteTextBox(codeNum))
  } else {
    self.ReasonReact.send(UpdateAndCompile(codeNum, content))
  }
};

type ocamlAST;
type compiledJS;
[@bs.val] external parseRE : string => ocamlAST = "";
[@bs.val] external printRE : ocamlAST => string = "";
[@bs.val] external printML : ocamlAST => string = "";
[@bs.scope "ocaml"] [@bs.val] external ocamlToJS : string => compiledJS = "compile";
[@bs.get] external getJSCode : compiledJS => string = "js_code";
[@bs.get] [@bs.return nullable] external getJSError : compiledJS => option(string) = "js_error_msg";
type processRowT = rowT => svgRectT;
[@bs.val] [@bs.return nullable] external evalJS : string => option(processRowT) = "eval";

let preamble = {|
type rowT = {width: int, height: int, color: string};
type svgRectT = {x: int, y: int, w: int, h: int, c: string};
let processRow = (row: rowT) : svgRectT => { row
|};


let processRow = (row) => {
  row
    |> (x) => {1}
    |> (x) => {2}
    |> (x) => {4}
};

let wrapFunction = (body) => Printf.sprintf("|> ((x) => {%s})", body);

let doCompile = (state) => {
  print_endline("Compiling...");
  let reason_code = String.concat("\n", List.map(wrapFunction, state.textList));
  let wrapped_code = preamble ++ reason_code ++ "}";
  /* print_endline(wrapped_code); */
  let maybeRE = try (Ok(parseRE(wrapped_code))) {
    | Js.Exn.Error(e) =>
      switch (Js.Exn.message(e)) {
        | Some(message) => Error(message)
        | None => Error("Unknown reason parsing error")
        }
  };
  switch (maybeRE) {
    | Ok(ocamlAST) =>
      let ocaml = printML(ocamlAST);
      /* print_endline(ocaml); */
      let js = ocamlToJS(ocaml);
      switch(getJSError(js)){
        | Some(error) =>
          let re = Js.Re.fromString("This expression has type [a-zA-Z]+ but an expression was expected of type\\s+svgRectT");
          if (Js.Re.test(error, re)){
            {...state, error: None, textList: List.rev(["", ...List.rev(state.textList)])}
          } else {
            {...state, error: Some(error)}
          }
        | None =>
            /* print_endline(getJSCode(js)); */
            switch (evalJS(getJSCode(js))){
              | None => {...state, error: Some("Error when evaling code")}
              | Some(processRow) => Js.log(processRow);
              {...state, output: Some(Array.map(processRow, state.data)), error: None}
            }
      }

    | Error(msg) => {...state, error: Some(msg)}
  }
};

let createRow = (id, row: rowT) : ReasonReact.reactElement => {
  <tr key=string_of_int(id)>
    <td style=ReactDOMRe.Style.make(~border="1px solid black", ())>(ReasonReact.stringToElement(string_of_int(row.width)))</td>
    <td style=ReactDOMRe.Style.make(~border="1px solid black", ())>(ReasonReact.stringToElement(string_of_int(row.height)))</td>
    <td style=ReactDOMRe.Style.make(~border="1px solid black", ())>(ReasonReact.stringToElement(row.color))</td>
  </tr>
};

let make = (_children) => {
  ...component,
  initialState: () => {
    textList: ["{x: 1, y: 1, w: row.width, h: row.height, c: row.color}"],
    error: None,
    output: None,
    data: [|
      {width: 20, height: 40, color: "blue"},
      {width: 40, height: 30, color: "red"},
    |]},
  reducer: (action, state) =>
    switch (action) {
    | UpdateAndCompile(textNum, text : string) =>
        let newTextList : list(string) = List.mapi((i, x) => if (i === textNum) {text} else {x}, state.textList);
        ReasonReact.Update(doCompile({...state, textList: newTextList}));
    | DeleteTextBox(textNum) =>
        if (List.length(state.textList) === 1){
          ReasonReact.Update({...state, textList: [""]});
        } else {
          let newTextList : list(string) = filteri((i, x) => i !== textNum, state.textList);
          ReasonReact.Update({...state, textList: newTextList});
        }
    },
  render: self =>
    <div>
      <table style=ReactDOMRe.Style.make(~border="1px solid black", ())>
      <thead>
        <tr>
          <th>(ReasonReact.stringToElement("width"))</th>
          <th>(ReasonReact.stringToElement("height"))</th>
          <th>(ReasonReact.stringToElement("color"))</th>
        </tr>
      </thead>
      <tbody>
      (ReasonReact.arrayToElement(Array.mapi(createRow, self.state.data)))
      </tbody>
      </table>
      <br/>
      <br/>
      (ReasonReact.arrayToElement(Array.mapi((i, text) => (
        <div key=string_of_int(i)>
          (ReasonReact.stringToElement("(x) => {"))
          <br/>
          <textarea name="textarea"
              style=ReactDOMRe.Style.make(~marginLeft="30px", ~marginTop="5px", ~height="100px", ~width="300px", ())
              value={text}
              onChange=(event => handleText(i, event, self)) ></textarea>
          <br/>
          (ReasonReact.stringToElement("}"))
          <br/>
        </div>
        ), Array.of_list(self.state.textList))))
      <br/>
      (switch (self.state.error) {
        | None => ReasonReact.nullElement
        | Some(error) => <div><br />(ReasonReact.stringToElement(error))</div>
      })
      <svg width="100" height="100">
        (switch (self.state.output) {
          | None => ReasonReact.nullElement
          | Some(output) => ReasonReact.arrayToElement(Array.mapi((i, svgRect) =>
          (<rect
              key=string_of_int(i)
              width=string_of_int(svgRect.w)
              height=string_of_int(svgRect.h)
              style=ReactDOMRe.Style.make(~fill=svgRect.c, ()) />), output))
        })
      </svg>
    </div>,
};
