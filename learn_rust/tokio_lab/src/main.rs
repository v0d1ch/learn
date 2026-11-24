#[tokio::main]
async fn main() {
    println!("Hello Tokio");
    let first = tokio::spawn(fetch_todo(1));
    let second = tokio::spawn(fetch_todo(2));
    let first_result = first.await.expect("First task failed");
    let second_result = second.await.expect("Second task failed");
    match first_result {
        Ok(response) => {
            match response.text().await {
                Ok(body) =>  println!("{body}"),
                Err(e) => eprintln!("Error in body {e}"),

            }
        }
        Err(e) => eprintln!("Error {e}"),
    }
    match second_result {
        Ok(response) => {
            match response.text().await {
                Ok(body) =>  println!("{body}"),
                Err(e) => eprintln!("Error in body {e}"),

            }
        }
        Err(e) => eprintln!("Error {e}"),
    }
}

async fn fetch_todo(id: u32) -> Result<reqwest::Response, reqwest::Error> {
    let url = format!("https://jsonplaceholder.typicode.com/todos/{id}");
    reqwest::get(&url).await
}
