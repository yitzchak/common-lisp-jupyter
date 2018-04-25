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
    code_hello_world = 'printf(stdout, "hello, world")$'

    # code which should cause (any) text to be written to STDERR
    code_stderr = 'printf(stderr, "test")$'

    # samples for testing code-completeness (used by console only)
    # these samples should respectively be unambigiously complete statements
    # (which should be executed on <enter>), incomplete statements or code
    # which should be identified as invalid
    complete_code_samples = [
        'x;',
        ':br foo',
        ":lisp (mapcar #'car '((1 a) (2 b) (3 c)))",
        '??erfc',
    ]
    incomplete_code_samples = [
        'x',
        ":lisp (mapcar #'car '((1 a) (2 b) (3 c))",
    ]
    invalid_code_samples = [
        'foo(;',
    ]

    # Pager: code that should display something (anything) in the pager
    code_page_something = "??erfc"

    # code which should generate a (user-level) error in the kernel, and send
    # a traceback to the client
    code_generate_error = ':lisp (maxima-jupyter::make-error-result "foo" "bar")'

    # Samples of code which generate a result value (ie, some text
    # displayed as Out[n])
    code_execute_result = [
        {'code': 'display2d:false$ 6*7;', 'result': '(%o26) 42'}
    ]

    # Samples of code which should generate a rich display output, and
    # the expected MIME type
    code_display_data = [{  # plot2d tests
        'code': 'plot2d(x^2, [x,-1,1], [svg_file, "a.svg"])$',
        'mime': 'image/svg+xml'
    }, {
        'code': 'plot2d(x^2, [x,-1,1], [pdf_file, "a.pdf"])$',
        'mime': 'application/pdf'
    }, {
        'code': 'plot2d(x^2, [x,-1,1], [ps_file, "a.ps"])$',
        'mime': 'application/postscript'
    }, {
        'code': 'plot2d(x^2, [x,-1,1], [png_file, "a.png"])$',
        'mime': 'image/png'
    }, {  # plot3d tests
        'code': 'plot3d(x^2+y, [x,-1,1], [y,-1,1], [svg_file, "a.svg"])$',
        'mime': 'image/svg+xml'
    }, {  # implicit_plot tests
        'code': 'load(implicit_plot)$ implicit_plot (x^2 = y^3 - 3*y + 1, [x, -4, 4], [y, -4, 4], [svg_file, "a.svg"])$',
        'mime': 'image/svg+xml'
    }, {
        'code': 'implicit_plot (x^2 = y^3 - 3*y + 1, [x, -4, 4], [y, -4, 4], [pdf_file, "a.pdf"])$',
        'mime': 'application/pdf'
    }, {
        'code': 'implicit_plot (x^2 = y^3 - 3*y + 1, [x, -4, 4], [y, -4, 4], [ps_file, "a.ps"])$',
        'mime': 'application/postscript'
    }, {
        'code': 'implicit_plot (x^2 = y^3 - 3*y + 1, [x, -4, 4], [y, -4, 4], [png_file, "a.png"])$',
        'mime': 'image/png'
    }, { # julia test
        'code': 'julia (-0.55, 0.6, [x, -0.3, 0.2], [y, 0.3, 0.9], [svg_file, "a.svg"])$',
        'mime': 'image/svg+xml'
    }, { # mandelbrot test
        'code': 'mandelbrot ([x, -2, 1], [y, -1.2, 1.2], [svg_file, "a.svg"])$',
        'mime': 'image/svg+xml'
    }, { # draw tests
        'code': 'draw(terminal=svg,gr3d(explicit(x^2+y^2,x,-1,1,y,-1,1)))$',
        'mime': 'image/svg+xml'
    }, {
        'code': 'draw(terminal=png,gr3d(explicit(x^2+y^2,x,-1,1,y,-1,1)))$',
        'mime': 'image/png'
    }, {
        'code': 'draw(terminal=eps,gr3d(explicit(x^2+y^2,x,-1,1,y,-1,1)))$',
        'mime': 'application/postscript'
    }, {
        'code': 'draw(terminal=jpg,gr3d(explicit(x^2+y^2,x,-1,1,y,-1,1)))$',
        'mime': 'image/jpeg'
    }, {
        'code': 'draw(terminal=pdf,gr3d(explicit(x^2+y^2,x,-1,1,y,-1,1)))$',
        'mime': 'application/pdf'
    }, {
        'code': 'draw(terminal=gif,gr3d(explicit(x^2+y^2,x,-1,1,y,-1,1)))$',
        'mime': 'image/gif'
    }, { # conv function tests
        'code': 'jupyter_html("<html/>", true);',
        'mime': 'text/html'
    }, {
        'code': 'jupyter_latex("$$x$$", true);',
        'mime': 'text/latex'
    }, {
        'code': 'jupyter_markdown("x", true);',
        'mime': 'text/markdown'
    }, {
        'code': 'jupyter_svg("<svg/>", true);',
        'mime': 'image/svg+xml'
    }, {
        'code': 'jupyter_text("x", true);',
        'mime': 'text/plain'
    }]

    # def test_maxima_latex(self):
    #     reply, output_msgs = self.execute_helper(code='solve(x^2+x+1=0,x);')
    #     print(reply)
    #     print(output_msgs)
    #     self.assertEqual(output_msgs[0]['msg_type'], 'stream')
    #     self.assertEqual(output_msgs[0]['content']['name'], 'stderr')
    #     self.assertEqual(output_msgs[0]['content']['text'], 'oops\n')


if __name__ == '__main__':
    unittest.main()
