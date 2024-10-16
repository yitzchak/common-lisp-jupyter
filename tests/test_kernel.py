import json
import pathlib
import pytest


JSON_DATA = {"fu": 1, "bar": [2, 3]}

JSON_MIME_TYPE = "application/json"
VEGA_MIME_TYPE = "application/vnd.vega.v5+json"
VEGA_LITE_MIME_TYPE = "application/vnd.vegalite.v4+json"


def sanitize_path(p):
    return p, str(pathlib.Path(p)).replace("\\", "\\\\")


@pytest.fixture(
    params=[
        "common-lisp",
        #"common-lisp_abcl",
        "common-lisp_ccl",
        "common-lisp_clasp",
        "common-lisp_clisp",
        "common-lisp_cmu",
        "common-lisp_ecl",
        "common-lisp_sbcl",
    ],
    scope="module",
)
def jupyter_kernel(jupyter_kernel):
    return jupyter_kernel


def test_hello_world(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(write-string "hello, world")',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "stream",
                    "content": {"name": "stdout", "text": "hello, world"},
                }
            ]
        ],
    )


def test_goodbye_world(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(write-string "goodbye, world" *error-output*)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "stream",
                    "content": {"name": "stderr", "text": "goodbye, world"},
                }
            ]
        ],
    )


def test_execute(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        "(1+ 7)",
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "8"}},
                }
            ]
        ],
    )


def test_execute_previous_results(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        "(values 'a1 'a2) 'b (values 'c1 'c2 'c3) (list / // ///)",
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "A1"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "A2"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "B"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "C1"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "C2"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "C3"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "((C1 C2 C3) (B) (A1 A2))"}},
                },
            ]
        ],
    )


def test_execute_previous_primary_results(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        "(values 'a1 'a2) 'b (values 'c1 'c2 'c3) (list * ** ***)",
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "A1"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "A2"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "B"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "C1"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "C2"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "C3"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "(C1 B A1)"}},
                },
            ]
        ],
    )


def test_execute_previous_eval_forms(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        "(+ 0 1) (- 4 2) (/ 9 3) (list + ++ +++)",
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "1"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "2"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "3"}},
                },
                {
                    "msg_type": "execute_result",
                    "content": {"data": {"text/plain": "((/ 9 3) (- 4 2) (+ 0 1))"}},
                },
            ]
        ],
    )


def test_execute_error(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        "(/ 1 0)",
        timeout=10,
        stop_on_error=False,
        expected_reply=[{"content": {"status": "error", "ename": "DIVISION-BY-ZERO"}}],
    )


def test_execute_result_error(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        """(defclass fu () ())
           (defmethod print-object ((object fu) stream)
             (declare (ignore fu stream))
             (error "qux"))
           (print (make-instance 'fu))""",
        timeout=10,
        stop_on_error=False,
        expected_reply=[
            {"content": {"status": "error", "ename": "SIMPLE-ERROR", "evalue": "qux"}}
        ],
    )


def test_execute_clear_output(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        "(j:clear)",
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "clear_output",
                    "content": {"wait": False},
                },
            ]
        ],
    )


def test_execute_clear_output_wait(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        "(j:clear t)",
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "clear_output",
                    "content": {"wait": True},
                },
            ]
        ],
    )


def test_execute_page(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(write-string "foo" j:*page-output*)',
        timeout=10,
        expected_reply=[
            {
                "content": {
                    "status": "ok",
                    "payload": [
                        {"source": "page", "data": {"text/plain": "foo"}, "start": 0}
                    ],
                }
            }
        ],
    )


def test_execute_ask_exit(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        "(j:quit)",
        timeout=10,
        expected_reply=[
            {
                "content": {
                    "status": "ok",
                    "payload": [{"source": "ask_exit", "keepkernel": False}],
                }
            }
        ],
    )


def test_execute_ask_exit_keepkernel(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        "(j:quit t)",
        timeout=10,
        expected_reply=[
            {
                "content": {
                    "status": "ok",
                    "payload": [{"source": "ask_exit", "keepkernel": True}],
                }
            }
        ],
    )


def test_execute_edit(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(j:edit "wibble")',
        timeout=10,
        expected_reply=[
            {
                "content": {
                    "status": "ok",
                    "payload": [
                        {"source": "edit_magic", "filename": "wibble", "line_number": 0}
                    ],
                }
            }
        ],
    )


def test_execute_edit_line_number(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(j:edit "wibble" 743)',
        timeout=10,
        expected_reply=[
            {
                "content": {
                    "status": "ok",
                    "payload": [
                        {
                            "source": "edit_magic",
                            "filename": "wibble",
                            "line_number": 743,
                        }
                    ],
                }
            }
        ],
    )


def test_display_data_text(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(jupyter:text "wibble" :display t)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "display_data",
                    "content": {"data": {"text/plain": "wibble"}},
                },
            ]
        ],
    )


def test_display_data_markdown(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(jupyter:markdown "wibble" :display t)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "display_data",
                    "content": {"data": {"text/markdown": "wibble"}},
                },
            ]
        ],
    )


def test_display_data_markdown_output(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(write-string "wibble" j:*markdown-output*)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "display_data",
                    "content": {"data": {"text/markdown": "wibble"}},
                },
            ]
        ],
    )


def test_display_data_html(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(jupyter:html "<html/>" :display t)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "display_data",
                    "content": {"data": {"text/html": "<html/>"}},
                },
            ]
        ],
    )


def test_display_data_html_output(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(write-string "<html/>" j:*html-output*)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "display_data",
                    "content": {"data": {"text/html": "<html/>"}},
                },
            ]
        ],
    )


def test_display_data_svg(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(jupyter:svg "<svg/>" :display t)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "display_data",
                    "content": {"data": {"image/svg+xml": "<svg/>"}},
                },
            ]
        ],
    )


def test_display_data_json(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(jupyter:json \'(:object-plist "fu" 1 "bar" #(2 3)) :display t)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "display_data",
                    "content": {
                        "data": {"application/json": {"fu": 1, "bar": [2, 3]}},
                        "metadata": {"application/json": {"expanded": False}},
                    },
                },
            ]
        ],
    )


def test_display_data_json_expanded(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(jupyter:json \'(:object-plist "fu" 1 "bar" #(2 3)) :display t :expanded t)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "display_data",
                    "content": {
                        "data": {"application/json": {"fu": 1, "bar": [2, 3]}},
                        "metadata": {"application/json": {"expanded": True}},
                    },
                },
            ]
        ],
    )


def test_display_data_json_file(jupyter_kernel, tmp_path):
    p, s = sanitize_path(tmp_path / "t.json")
    p.write_text(json.dumps(JSON_DATA))
    jupyter_kernel.execute_read_reply(
        f'(jupyter:json-file "{s}" :display t)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "display_data",
                    "content": {
                        "data": dict([[JSON_MIME_TYPE, JSON_DATA]]),
                        "metadata": dict([[JSON_MIME_TYPE, {"expanded": False}]]),
                    },
                },
            ]
        ],
    )


def test_display_data_json_file_expanded(jupyter_kernel, tmp_path):
    p, s = sanitize_path(tmp_path / "t.json")
    p.write_text(json.dumps(JSON_DATA))
    jupyter_kernel.execute_read_reply(
        f'(jupyter:json-file "{s}" :display t :expanded t)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "display_data",
                    "content": {
                        "data": dict([[JSON_MIME_TYPE, JSON_DATA]]),
                        "metadata": dict([[JSON_MIME_TYPE, {"expanded": True}]]),
                    },
                },
            ]
        ],
    )


def test_display_data_vega_file(jupyter_kernel, tmp_path):
    p, s = sanitize_path(tmp_path / "t.json")
    p.write_text(json.dumps(JSON_DATA))
    jupyter_kernel.execute_read_reply(
        f'(jupyter:vega-file "{s}" :display t)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "display_data",
                    "content": {"data": dict([[VEGA_MIME_TYPE, JSON_DATA]])},
                },
            ]
        ],
    )


def test_display_data_vega_lite_file(jupyter_kernel, tmp_path):
    p, s = sanitize_path(tmp_path / "t.json")
    p.write_text(json.dumps(JSON_DATA))
    jupyter_kernel.execute_read_reply(
        f'(jupyter:vega-lite-file "{s}" :display t)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [
                {
                    "msg_type": "display_data",
                    "content": {"data": dict([[VEGA_LITE_MIME_TYPE, JSON_DATA]])},
                },
            ]
        ],
    )


def test_display_data_latex(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        '(jupyter:latex "$r^2$" :display t)',
        timeout=10,
        expected_reply=[{"content": {"status": "ok"}}],
        expected_messages=[
            [{"msg_type": "display_data", "content": {"data": {"text/latex": "$r^2$"}}}]
        ],
    )


def test_kernel_info(jupyter_kernel):
    jupyter_kernel.kernel_info_read_reply(
        timeout=10,
        expected_reply=[{"content": {"status": "ok", "implementation": "common-lisp"}}],
    )


def test_comm_info(jupyter_kernel):
    jupyter_kernel.comm_info_read_reply(timeout=10)


def test_comm_open_fail(jupyter_kernel):
    jupyter_kernel.comm_open_read_reply(
        comm_id="wibble",
        target_name="fubar",
        timeout=10,
        expected_messages=[
            [{"msg_type": "comm_close", "content": {"comm_id": "wibble", "data": {}}}]
        ],
    )


# def test_comm_msg_fail(jupyter_kernel):
#     jupyter_kernel.comm_msg_read_reply(
#         comm_id="wibble",
#         timeout=10,
#         expected_messages=[
#             [
#                 {
#                     "msg_type": "stream",
#                     "content": {
#                         "name": "stderr",
#                         "text": "<KERNEL> Received COMM message with unknown comm_id of wibble.\n",
#                     },
#                 }
#             ]
#         ],
#     )


def test_is_complete(jupyter_kernel):
    jupyter_kernel.is_complete_read_reply(
        "(fu bar)", timeout=10, expected_reply=[{"content": {"status": "complete"}}]
    )


def test_is_invalid(jupyter_kernel):
    jupyter_kernel.is_complete_read_reply(
        "(fu bar))", timeout=10, expected_reply=[{"content": {"status": "invalid"}}]
    )


def test_is_incomplete(jupyter_kernel):
    jupyter_kernel.is_complete_read_reply(
        "(fu bar", timeout=10, expected_reply=[{"content": {"status": "incomplete"}}]
    )


def test_complete_z(jupyter_kernel):
    jupyter_kernel.complete_read_reply(
        "(z",
        timeout=10,
        expected_reply=[
            {
                "content": {
                    "status": "ok",
                    "matches": {"zerop"},
                    "cursor_start": 1,
                    "cursor_end": 2,
                    "metadata": {
                        "_jupyter_types_experimental": [
                            {"type": "function", "text": "zerop"},
                        ]
                    },
                }
            }
        ],
    )


def test_complete_variable(jupyter_kernel):
    jupyter_kernel.complete_read_reply(
        "*re",
        timeout=10,
        expected_reply=[
            {
                "content": {
                    "status": "ok",
                    "matches": {
                        "*read-base*",
                        "*read-default-float-format*",
                        "*read-eval*",
                        "*read-suppress*",
                        "*readtable*",
                    },
                    "cursor_start": 0,
                    "cursor_end": 3,
                    "metadata": {
                        "_jupyter_types_experimental": (
                            {"type": "variable", "text": "*read-base*"},
                            {"type": "variable", "text": "*read-default-float-format*"},
                            {"type": "variable", "text": "*read-eval*"},
                            {"type": "variable", "text": "*read-suppress*"},
                            {"type": "variable", "text": "*readtable*"},
                        )
                    },
                }
            }
        ],
    )


def test_complete_indent(jupyter_kernel):
    jupyter_kernel.complete_read_reply(
        "(unwind-protect\nfu\nbar)",
        timeout=10,
        expected_reply=[
            {
                "content": {
                    "status": "ok",
                    "matches": ["(unwind-protect\n    fu\n  bar)"],
                    "cursor_start": 0,
                    "cursor_end": 23,
                }
            }
        ],
    )


def test_inspect(jupyter_kernel):
    jupyter_kernel.inspect_read_reply(
        "format",
        timeout=10,
        expected_reply=[
            {
                "content": {
                    "status": "ok",
                    "found": True,
                }
            }
        ],
    )


def test_history_tail(jupyter_kernel):
    jupyter_kernel.execute_read_reply(
        "1", timeout=10, expected_reply=[{"content": {"status": "ok"}}]
    )
    jupyter_kernel.execute_read_reply(
        "2", timeout=10, expected_reply=[{"content": {"status": "ok"}}]
    )
    jupyter_kernel.execute_read_reply(
        "3", timeout=10, expected_reply=[{"content": {"status": "ok"}}]
    )
    jupyter_kernel.execute_read_reply(
        "4", timeout=10, expected_reply=[{"content": {"status": "ok"}}]
    )
    jupyter_kernel.execute_read_reply(
        "5", timeout=10, expected_reply=[{"content": {"status": "ok"}}]
    )
    jupyter_kernel.history_read_reply(
        hist_access_type="tail",
        n=5,
        timeout=10,
        expected_reply=[
            {
                "content": {
                    "status": "ok",
                    "history": [
                        [int, int, "1"],
                        [int, int, "2"],
                        [int, int, "3"],
                        [int, int, "4"],
                        [int, int, "5"],
                    ],
                },
            }
        ],
    )


# def test_widget_button(jupyter_kernel):
#     reply, messages = jupyter_kernel.execute_read_reply(
#         '''(jw:make-button :description "fubar"
#                            :on-click (list (lambda (inst)
#                                              (declare (ignore inst))
#                                              (write-string "wibble")
#                                              (finish-output)
#                                              (error "gronk"))))''',
#         timeout=10,
#         expected_messages=[
#             (
#                 {
#                     "msg_type": "comm_open",
#                     "metadata": {"version": "2.0.0"},
#                     "content": {
#                         "target_name": "jupyter.widget",
#                         "data": {
#                             "state": {
#                                 "button_color": None,
#                                 "_view_module_version": "1.2.0",
#                                 "_view_module": "@jupyter-widgets/base",
#                                 "_view_name": "StyleView",
#                                 "_model_module_version": "1.5.0",
#                                 "_model_module": "@jupyter-widgets/controls",
#                                 "_model_name": "ButtonStyleModel",
#                             },
#                             "buffer_paths": [],
#                         },
#                     },
#                 },
#                 {
#                     "msg_type": "comm_open",
#                     "metadata": {"version": "2.0.0"},
#                     "content": {
#                         "target_name": "jupyter.widget",
#                         "data": {
#                             "state": {
#                                 "width": None,
#                                 "visibility": None,
#                                 "top": None,
#                                 "right": None,
#                                 "padding": None,
#                                 "overflow_y": None,
#                                 "overflow_x": None,
#                                 "overflow": None,
#                                 "order": None,
#                                 "object_position": None,
#                                 "object_fit": None,
#                                 "min_width": None,
#                                 "min_height": None,
#                                 "max_width": None,
#                                 "max_height": None,
#                                 "margin": None,
#                                 "left": None,
#                                 "justify_items": None,
#                                 "justify_content": None,
#                                 "height": None,
#                                 "grid_template_rows": None,
#                                 "grid_template_columns": None,
#                                 "grid_template_areas": None,
#                                 "grid_row": None,
#                                 "grid_gap": None,
#                                 "grid_column": None,
#                                 "grid_auto_rows": None,
#                                 "grid_auto_flow": None,
#                                 "grid_auto_columns": None,
#                                 "grid_area": None,
#                                 "flex_flow": None,
#                                 "flex": None,
#                                 "display": None,
#                                 "bottom": None,
#                                 "border": None,
#                                 "align_self": None,
#                                 "align_items": None,
#                                 "align_content": None,
#                                 "_view_module_version": "1.2.0",
#                                 "_view_module": "@jupyter-widgets/base",
#                                 "_view_name": "LayoutView",
#                                 "_model_module_version": "1.2.0",
#                                 "_model_module": "@jupyter-widgets/base",
#                                 "_model_name": "LayoutModel",
#                             },
#                             "buffer_paths": [],
#                         },
#                     },
#                 },
#                 {
#                     "msg_type": "comm_open",
#                     "metadata": {"version": "2.0.0"},
#                     "content": {
#                         "target_name": "jupyter.widget",
#                         "data": {
#                             "state": {
#                                 "description": "fubar",
#                                 "_view_module_version": "1.5.0",
#                                 "_view_module": "@jupyter-widgets/controls",
#                                 "_view_name": "ButtonView",
#                                 "_model_module_version": "1.5.0",
#                                 "_model_module": "@jupyter-widgets/controls",
#                                 "_model_name": "ButtonModel",
#                                 "button_style": "",
#                                 "disabled": False,
#                                 "icon": "",
#                                 "tooltip": "",
#                             },
#                             "buffer_paths": [],
#                         },
#                     },
#                 },
#                 {
#                     "msg_type": "execute_result",
#                     "content": {
#                         "data": {
#                             "application/vnd.jupyter.widget-view+json": {
#                                 "version_major": 2,
#                                 "version_minor": 0,
#                             },
#                         },
#                         "metadata": {},
#                     },
#                 }
#             ),
#         ],
#     )
#     jupyter_kernel.comm_msg_read_reply(
#         comm_id=messages[2]["content"]["comm_id"],
#         data={"method": "custom", "content": {"event": "click"}},
#         timeout=10,
#         expected_messages=[
#             (
#                 {
#                     "msg_type": "stream",
#                     "content": {"name": "stdout", "text": "wibble"},
#                 },
#                 {
#                     "msg_type": "stream",
#                     "content": {"name": "stderr", "text": "SIMPLE-ERROR: gronk\n\n"},
#                 }
#             )
#         ],
#     )


    
