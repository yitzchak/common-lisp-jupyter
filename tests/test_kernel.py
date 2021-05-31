import pytest


@pytest.fixture(
    params=[
        "common-lisp",
        "common-lisp_abcl",
        "common-lisp_ccl",
        "common-lisp_clasp",
        "common-lisp_cmu",
        "common-lisp_ecl",
        "common-lisp_sbcl",
    ],
    scope="module",
)
def jupyter_kernel(jupyter_kernel):
    return jupyter_kernel


def test_hello_world(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(write-string "hello, world")',
        timeout=10,
        expected_reply_status="ok",
        expected_stream=[{"name": "stdout", "text": "hello, world"}],
    )


def test_goodbye_world(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(write-string "goodbye, world" *error-output*)',
        timeout=10,
        expected_reply_status="ok",
        expected_stream=[{"name": "stderr", "text": "goodbye, world"}],
    )


def test_execute(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        "(1+ 7)",
        timeout=10,
        expected_reply_status="ok",
        expected_execute_result=[{"data": {"text/plain": "8"}}],
    )


def test_execute_previous_results(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        "(values 'a1 'a2) 'b (values 'c1 'c2 'c3) (list / // ///)",
        timeout=10,
        expected_reply_status="ok",
        expected_execute_result=[
            {"data": {"text/plain": "A1"}},
            {"data": {"text/plain": "A2"}},
            {"data": {"text/plain": "B"}},
            {"data": {"text/plain": "C1"}},
            {"data": {"text/plain": "C2"}},
            {"data": {"text/plain": "C3"}},
            {"data": {"text/plain": "((C1 C2 C3) (B) (A1 A2))"}}],
    )


def test_execute_previous_primary_results(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        "(values 'a1 'a2) 'b (values 'c1 'c2 'c3) (list * ** ***)",
        timeout=10,
        expected_reply_status="ok",
        expected_execute_result=[
            {"data": {"text/plain": "A1"}},
            {"data": {"text/plain": "A2"}},
            {"data": {"text/plain": "B"}},
            {"data": {"text/plain": "C1"}},
            {"data": {"text/plain": "C2"}},
            {"data": {"text/plain": "C3"}},
            {"data": {"text/plain": "(C1 B A1)"}}],
    )


def test_execute_previous_eval_forms(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        "(+ 0 1) (- 4 2) (/ 9 3) (list + ++ +++)",
        timeout=10,
        expected_reply_status="ok",
        expected_execute_result=[
            {"data": {"text/plain": "1"}},
            {"data": {"text/plain": "2"}},
            {"data": {"text/plain": "3"}},
            {"data": {"text/plain": "((/ 9 3) (- 4 2) (+ 0 1))"}}],
    )


def test_execute_error(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        "(/ 1 0)",
        timeout=10,
        expected_reply_status="error",
        expected_reply_ename="DIVISION-BY-ZERO",
    )


def test_execute_clear_output(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        "(j:clear)",
        timeout=10,
        expected_reply_status="ok",
        expected_clear_output=[{"wait": False}],
    )


def test_execute_clear_output_wait(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        "(j:clear t)",
        timeout=10,
        expected_reply_status="ok",
        expected_clear_output=[{"wait": True}],
    )


def test_execute_page(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(write-string "foo" j:*page-output*)',
        timeout=10,
        expected_reply_status="ok",
        expected_reply_payload=[
            {"source": "page", "data": {"text/plain": "foo"}, "start": 0}
        ],
    )


def test_display_data_text(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(jupyter:text "wibble" :display t)',
        timeout=10,
        expected_reply_status="ok",
        expected_display_data=[{"data": {"text/plain": "wibble"}}],
    )


def test_display_data_markdown(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(jupyter:markdown "wibble" :display t)',
        timeout=10,
        expected_reply_status="ok",
        expected_display_data=[{"data": {"text/markdown": "wibble"}}],
    )


def test_display_data_markdown_output(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(write-string "wibble" j:*markdown-output*)',
        timeout=10,
        expected_reply_status="ok",
        expected_display_data=[{"data": {"text/markdown": "wibble"}}],
    )


def test_display_data_html(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(jupyter:html "<html/>" :display t)',
        timeout=10,
        expected_reply_status="ok",
        expected_display_data=[{"data": {"text/html": "<html/>"}}],
    )


def test_display_data_html_output(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(write-string "<html/>" j:*html-output*)',
        timeout=10,
        expected_reply_status="ok",
        expected_display_data=[{"data": {"text/html": "<html/>"}}],
    )


def test_display_data_svg(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(jupyter:svg "<svg/>" :display t)',
        timeout=10,
        expected_reply_status="ok",
        expected_display_data=[{"data": {"image/svg+xml": "<svg/>"}}],
    )


def test_display_data_json(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(jupyter:json \'(:object-plist "fu" 1 "bar" #(2 3)) :display t)',
        timeout=10,
        expected_reply_status="ok",
        expected_display_data=[
            {
                "data": {"application/json": {"fu": 1, "bar": [2, 3]}},
                "metadata": {"application/json": {"expanded": False}},
            }
        ],
    )


def test_display_data_json_expanded(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(jupyter:json \'(:object-plist "fu" 1 "bar" #(2 3)) :display t :expanded t)',
        timeout=10,
        expected_reply_status="ok",
        expected_display_data=[
            {
                "data": {"application/json": {"fu": 1, "bar": [2, 3]}},
                "metadata": {"application/json": {"expanded": True}},
            }
        ],
    )


def test_display_data_latex(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(jupyter:latex "$r^2$" :display t)',
        timeout=10,
        expected_reply_status="ok",
        expected_display_data=[{"data": {"text/latex": "$r^2$"}}],
    )


def test_kernel_info(jupyter_kernel):
    reply, messages = jupyter_kernel.kernel_info_read_reply(
        timeout=10, expected_reply_status="ok"
    )
    assert reply["content"]["implementation"] == "common-lisp"


def test_comm_info(jupyter_kernel):
    reply, messages = jupyter_kernel.comm_info_read_reply(timeout=10)


def test_is_complete(jupyter_kernel):
    reply, messages = jupyter_kernel.is_complete_read_reply(
        "(fu bar)", timeout=10, expected_reply_status="complete"
    )


def test_is_invalid(jupyter_kernel):
    reply, messages = jupyter_kernel.is_complete_read_reply(
        "(fu bar))", timeout=10, expected_reply_status="invalid"
    )


def test_is_incomplete(jupyter_kernel):
    reply, messages = jupyter_kernel.is_complete_read_reply(
        "(fu bar", timeout=10, expected_reply_status="incomplete"
    )


def test_complete_z(jupyter_kernel):
    reply, messages = jupyter_kernel.complete_read_reply(
        "(z",
        timeout=10,
        expected_reply_status="ok",
        expected_matches=[{"text": "zerop", "type": "function"}],
        expected_cursor_start=1,
        expected_cursor_end=2,
    )


def test_complete_variable(jupyter_kernel):
    reply, messages = jupyter_kernel.complete_read_reply(
        "*re",
        timeout=10,
        expected_reply_status="ok",
        expected_matches=[
            {"text": "*read-base*", "type": "variable"},
            {"text": "*read-default-float-format*", "type": "variable"},
            {"text": "*read-eval*", "type": "variable"},
            {"text": "*read-suppress*", "type": "variable"},
            {"text": "*readtable*", "type": "variable"},
        ],
        expected_cursor_start=0,
        expected_cursor_end=3,
    )


def test_complete_indent(jupyter_kernel):
    reply, messages = jupyter_kernel.complete_read_reply(
        "(unwind-protect\nfu\nbar)",
        timeout=10,
        expected_reply_status="ok",
        expected_matches=[{"text": "(unwind-protect\n    fu\n  bar)"}],
        expected_cursor_start=0,
        expected_cursor_end=23,
    )


def test_inspect(jupyter_kernel):
    reply, messages = jupyter_kernel.inspect_read_reply(
        "format",
        timeout=10,
        expected_reply_status="ok",
        expected_reply_found=True,
    )



