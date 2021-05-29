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
        expected_stdout="hello, world",
    )


def test_goodbye_world(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(write-string "goodbye, world" *error-output*)',
        timeout=10,
        expected_reply_status="ok",
        expected_stderr="goodbye, world",
    )


def test_execute(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        "(1+ 7)",
        timeout=10,
        expected_reply_status="ok",
        expected_execute_results=[{"data": {"text/plain": "8"}}],
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


def test_display_data_html(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply(
        '(jupyter:html "<html/>" :display t)',
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
