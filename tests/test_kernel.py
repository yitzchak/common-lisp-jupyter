def test_execute(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply("(1+ 7)", timeout = 10)
    assert any(msg['msg_type'] == 'execute_result' and msg['content']['data']['text/plain'] == '8' for msg in messages), "wibble"


def test_kernel_info(jupyter_kernel):
    reply, messages = jupyter_kernel.kernel_info_read_reply(timeout = 10)
    assert reply['content']['implementation'] == 'common-lisp'


def test_comm_info(jupyter_kernel):
    reply, messages = jupyter_kernel.comm_info_read_reply(timeout = 10)


def test_is_complete(jupyter_kernel):
    reply, messages = jupyter_kernel.is_complete_read_reply("(fu bar)", timeout = 10)
    assert reply['content']['status'] == 'complete'

    reply, messages = jupyter_kernel.is_complete_read_reply("(fu bar", timeout = 10)
    assert reply['content']['status'] == 'incomplete'

