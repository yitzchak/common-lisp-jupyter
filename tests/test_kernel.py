import pytest


@pytest.fixture(params=['common-lisp', 'common-lisp_abcl', 'common-lisp_ccl',
                        'common-lisp_clasp', 'common-lisp_cmu',
                        'common-lisp_ecl', 'common-lisp_sbcl'],
                scope='module')
def jupyter_kernel(jupyter_kernel):
    return jupyter_kernel


def test_hello_world(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply("(write-string \"hello, world\")", timeout = 10)
    assert any(msg['msg_type'] == 'stream' and msg['content']['name'] == 'stdout'
               and msg['content']['text'] == 'hello, world' for msg in messages), (
           'Did not receive "hello, world" on stdout stream.')


def test_goodbye_world(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply("(write-string \"goodbye, world\" *error-output*)", timeout = 10)
    assert any(msg['msg_type'] == 'stream' and msg['content']['name'] == 'stderr'
               and msg['content']['text'] == 'goodbye, world' for msg in messages), (
           'Did not receive "goodbye, world" on stderr stream.')


def test_execute(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply("(1+ 7)", timeout = 10)
    assert any(msg['msg_type'] == 'execute_result' and msg['content']['data']['text/plain'] == '8' for msg in messages), (
           'Did not receive an execute_result with a value of "8".')


def test_kernel_info(jupyter_kernel):
    reply, messages = jupyter_kernel.kernel_info_read_reply(timeout = 10)
    assert reply['content']['implementation'] == 'common-lisp'


def test_comm_info(jupyter_kernel):
    reply, messages = jupyter_kernel.comm_info_read_reply(timeout = 10)


def test_is_complete(jupyter_kernel):
    reply, messages = jupyter_kernel.is_complete_read_reply("(fu bar)", timeout = 10)
    assert reply['content']['status'] == 'complete', (
           f'Expected a status of "complete" but received "{reply["content"]["status"]}."')


def test_is_invalid(jupyter_kernel):
    reply, messages = jupyter_kernel.is_complete_read_reply("(fu bar))", timeout = 10)
    assert reply['content']['status'] == 'invalid', (
           f'Expected a status of "invalid" but received "{reply["content"]["status"]}".')


def test_is_incomplete(jupyter_kernel):
    reply, messages = jupyter_kernel.is_complete_read_reply("(fu bar", timeout = 10)
    assert reply['content']['status'] == 'incomplete', (
           f'Expected a status of "incomplete" but received "{reply["content"]["status"]}".')


def test_complete_z(jupyter_kernel):
    reply, messages = jupyter_kernel.complete_read_reply("(z", timeout = 10)
    assert reply['content']['matches'] == ['zerop'], (
           f'Expected a single match of "zerop" but received "{reply["content"]["matches"]}."')
    assert reply['content']['cursor_start'] == 1, (
           f'Expected a cursor_start of 1 but received {reply["content"]["cursor_start"]}.')
    assert reply['content']['cursor_end'] == 2, (
           f'Expected a cursor_end of 2 but received {reply["content"]["cursor_end"]}.')
    assert reply['content']['metadata'] == {'_jupyter_types_experimental': [{'text': 'zerop', 'type': 'function'}]}, (
           f'Expected _jupyter_types_experimental to identify zerop as a function.')


def test_complete(jupyter_kernel):
    reply, messages = jupyter_kernel.complete_read_reply("(z", timeout = 10)
    assert reply['content']['matches'] == ['zerop'], (
           f'Expected a single match of "zerop" but received "{reply["content"]["matches"]}."')
    assert reply['content']['cursor_start'] == 1, (
           f'Expected a cursor_start of 1 but received {reply["content"]["cursor_start"]}.')
    assert reply['content']['cursor_end'] == 2, (
           f'Expected a cursor_end of 2 but received {reply["content"]["cursor_end"]}.')
    assert reply['content']['metadata'] == {'_jupyter_types_experimental': [{'text': 'zerop', 'type': 'function'}]}, (
           f'Expected _jupyter_types_experimental to identify zerop as a function.')


