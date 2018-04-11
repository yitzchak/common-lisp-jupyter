import unittest
import jupyter_kernel_test


class MyKernelTests(jupyter_kernel_test.KernelTests):
    # Required --------------------------------------

    # The name identifying an installed kernel to run the tests against
    kernel_name = "maxima"

    # language_info.name in a kernel_info_reply should match this
    language_name = "maxima"

    # the normal file extension (including the leading dot) for this language
    # checked against language_info.file_extension in kernel_info_reply
    file_extension = ".mac"

    # Optional --------------------------------------

    # Code in the kernel's language to write "hello, world" to stdout
    code_hello_world = 'print("hello, world");'

    # code which should cause (any) text to be written to STDERR
    code_stderr = 'printf(stderr, "test");'

    # samples for testing code-completeness (used by console only)
    # these samples should respectively be unambigiously complete statements
    # (which should be executed on <enter>), incomplete statements or code
    # which should be identified as invalid
    complete_code_samples = [
        'x;',
        ':br foo',
        ":lisp (mapcar #'car '((1 a) (2 b) (3 c)))",
        '??erfc'
    ]
    incomplete_code_samples = [
        'x',
        ":lisp (mapcar #'car '((1 a) (2 b) (3 c))"
    ]
    invalid_code_samples = [
        'foo(;'
    ]

    # Pager: code that should display something (anything) in the pager
    code_page_something = "??erfc"

    # code which should generate a (user-level) error in the kernel, and send
    # a traceback to the client
    code_generate_error = "error();"

    # Samples of code which generate a result value (ie, some text
    # displayed as Out[n])
    code_execute_result = [
        {'code': '6*7;', 'result': '42'}
    ]

    # Samples of code which should generate a rich display output, and
    # the expected MIME type
    code_display_data = [
        {
            'code': 'jupyter_html("<html/>", true);',
            'mime': 'text/html'
        },
        {
            'code': 'jupyter_latex("$$x$$", true);',
            'mime': 'text/latex'
        },
        {
            'code': 'jupyter_markdown("x", true);',
            'mime': 'text/markdown'
        },
        {
            'code': 'jupyter_svg("<svg/>", true);',
            'mime': 'image/svg+xml'
        },
        {
            'code': 'jupyter_text("x", true);',
            'mime': 'text/plain'
        }
    ]

    # def test_maxima_latex(self):
    #     reply, output_msgs = self.execute_helper(code='solve(x^2+x+1=0,x);')
    #     print(reply)
    #     print(output_msgs)
    #     self.assertEqual(output_msgs[0]['msg_type'], 'stream')
    #     self.assertEqual(output_msgs[0]['content']['name'], 'stderr')
    #     self.assertEqual(output_msgs[0]['content']['text'], 'oops\n')


if __name__ == '__main__':
    unittest.main()
