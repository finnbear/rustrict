use web_sys::{HtmlInputElement, window, InputEvent, HtmlTextAreaElement, wasm_bindgen::JsCast};
use yew::{html, Html, Callback, function_component, TargetCast};
use rustrict::Censor;

#[function_component(App)]
fn app() -> Html {
    let oninput = Callback::from(move |event: InputEvent| {
        if let Some(input) = event.target_dyn_into::<HtmlInputElement>() {
            let uncensored = input.value();
            let analysis_element = window().unwrap().document().unwrap().get_element_by_id("analysis").unwrap();
            let censored_element = window().unwrap().document().unwrap().get_element_by_id("censored").unwrap().dyn_into::<HtmlTextAreaElement>().unwrap();
            if uncensored.is_empty() {
                analysis_element.set_inner_html("N/A");
                censored_element.set_value("");
            } else {
                let mut censor = Censor::from_str(&uncensored);
                let (censored, analysis) = censor.censor_and_analyze();
                let count = censor.total_matches();
                let detections = censor.detections();
                let width = rustrict::width_str(&uncensored);  
                let result = format!("{analysis:?} (width = {width}, count = {count}, detections = {detections:?})");
                analysis_element.set_inner_html(&result);
                censored_element.set_value(&censored);
            }
        }
    });
    html! {<>
        <h2>{"Rustrict"}</h2>
        <h3>{"Input"}</h3>
        <input
            class="form-control"
            {oninput}
            type="text"
            style="background-color: #2c3e50; color: white; border-width: 0;"
        />
        <h3>{"Analysis"}</h3>
        <p id="analysis">{"N/A"}</p>
        <h3>{"Output"}</h3>
        <textarea
            id="censored"
            class="form-control"
            rows="10"
            readonly={true}
            tabindex="-1"
            style="background-color: #2c3e50; resize: vertical; color: white; border-width: 0; user-select: none;"
        ></textarea>
    </>}
}

/*
<script>
        async function censor() {
        const req = { text: document.getElementById("input").value };

        let response = await fetch("/", {
        method: "POST",
        body: JSON.stringify(req),
        headers: {
        "Content-Type": "application/json",
        },
        });
        let resp = await response.json();
        if (resp.original != req.text) {
        return;
        }
        document.getElementById(
        "analysis"
        ).innerHTML = `${resp.analysis} (width = ${resp.width})`;
        document.getElementById("output").value = resp.censored;
        }
        censor();
        </script>
*/

fn main() {
    yew::Renderer::<App>::new().render();
}