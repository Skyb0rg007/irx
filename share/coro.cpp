
#include <vector>
#include <iostream>
#include <coroutine>

struct Task {
    struct promise_type {
        Task get_return_object() {
            return { .h_ = std::coroutine_handle<promise_type>::from_promise(*this) };
        }
        std::suspend_never initial_suspend() { return {}; }
        std::suspend_never final_suspend() noexcept { return {}; }
        void unhandled_exception() {}
        std::suspend_always yield_value(int value) {
            value_ = value;
            return {};
        }
        int value_;
    };

    std::coroutine_handle<promise_type> h_;
};

Task evens(void)
{
    for (int i = 0;; i += 2)
        co_yield i;
}

int main(void)
{
    Task task = evens();
    Task::promise_type &promise = task.h_.promise();
    for (int i = 0; i < 200; i++) {
        std::cout << "evens: " << promise.value_ << std::endl;
        task.h_();
    }
    task.h_.destroy();
    return 0;
}
