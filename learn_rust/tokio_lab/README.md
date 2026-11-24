# Tokio lab

A small playground for learning async Rust by calling
[JSONPlaceholder](https://jsonplaceholder.typicode.com/), a free fake REST API
with no signup or API key. An internet connection is required.

## Run

From the `tokio_lab` directory:

```sh
cargo run -- single
cargo run -- sequential
cargo run -- concurrent
cargo run -- spawn
```

The first run downloads and compiles dependencies. Each request has a 10-second
timeout, and HTTP or JSON errors are returned to the terminal.

If this machine reports `cargo: command not found`, the installed toolchain can
be added to the current shell's PATH with:

```sh
export PATH="$HOME/.rustup/toolchains/1.93-aarch64-apple-darwin/bin:$PATH"
```

## Read the code

- `src/api.rs`: the `Todo` response type and an async HTTP function.
- `src/main.rs`: the Tokio runtime and four ways to drive requests.

Tokio runs futures and schedules tasks. Reqwest sends HTTP requests using Tokio,
and Serde converts the JSON response into a Rust struct.

Calling an `async fn` creates a future. Awaiting it lets it make progress and
yields to the runtime when it must wait. `.await` does not itself spawn a task.

Start with `single`, then compare `sequential` and `concurrent`: the former waits
for each request before starting the next; `tokio::try_join!` allows all three to
make progress together in one task. `spawn` uses `JoinSet` to run separate tasks
and print results in completion order. It uses `async move` because each spawned
task must own the data it needs. Dropping the set aborts any remaining tasks.

The printed duration includes network and TLS overhead. Public API caching and
network variation mean concurrent requests are not guaranteed to be faster in
every run; this is a learning example, not a benchmark.

## Experiments

1. Change the ID in `single` to another number from 1 to 200.
2. Try ID 9999 and inspect the HTTP error produced by `error_for_status()`.
3. Wrap `fetch_todo(&client, 1)` in
   `tokio::time::timeout(Duration::from_millis(1), ...)` and await it. It returns
   an outer timeout result containing the inner HTTP result. Increase the duration
   until the request succeeds; exact timing depends on the network.
4. Add `tokio::time::sleep(Duration::from_secs(1)).await` inside `fetch_todo`.
   Compare sequential and concurrent execution again. Avoid `std::thread::sleep`
   in async code because it blocks a runtime thread.
5. Extend `spawn` to fetch IDs 1 through 10, but allow at most three requests at
   once: initially spawn three, then spawn a replacement after each completion.
6. Add a function for `/posts/1` with a new response struct.

References: [Tokio tutorial](https://tokio.rs/tokio/tutorial),
[spawning tasks](https://tokio.rs/tokio/tutorial/spawning),
[Reqwest documentation](https://docs.rs/reqwest/latest/reqwest/).
