import unittest
import jupyter_kernel_test


class MyKernelTests(jupyter_kernel_test.KernelTests):
    @classmethod
    def tearDownClass(cls):
        cls.kc.shutdown()

    kernel_name = "common-lisp"

    language_name = "common-lisp"

    file_extension = ".lisp"

    code_hello_world = '(format t "hello, world")'

    code_stderr = '(format *error-output* "test")'

    complete_code_samples = [
        '\'x',
        '(+ 1 2)'
    ]
    incomplete_code_samples = [
        '(+ 1'
    ]
    invalid_code_samples = [
        '(+ 1 2))'
    ]

    code_page_something = "(format jupyter:*page-output* \"foo\")"

    # code_generate_error = '(/ 1 0)'

    code_execute_result = [
        {'code': '(+ 1 2)', 'result': '3'}
    ]

    code_display_data = [{
        'code': '(jupyter:text "wibble" t)',
        'mime': 'text/plain'
    }, {
        'code': '(jupyter:html "<html/>" t)',
        'mime': 'text/html'
    }, {
        'code': '(jupyter:latex "$r^2$" t)',
        'mime': 'text/latex'
    }, {
        'code': '(jupyter:markdown "wibble" t)',
        'mime': 'text/markdown'
    }, {
        'code': '(jupyter:svg "<svg/>" t)',
        'mime': 'image/svg+xml'
    }]

    code_inspect_sample = "format"

    code_clear_output = "(jupyter:clear)"


if __name__ == '__main__':
    unittest.main()
