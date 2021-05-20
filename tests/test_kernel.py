import jupyter_client
import pytest


def pytest_generate_tests(metafunc):
    if "jupyter_kernel" in metafunc.fixturenames:
        names = []
        mgr = jupyter_client.kernelspec.KernelSpecManager()
        for name, spec in mgr.get_all_specs().items():
            if spec['spec']['language'] == "common-lisp":
                names.append(name)
        metafunc.parametrize("jupyter_kernel", names, indirect=True, scope='module')


def test_hello_world(jupyter_kernel):
    reply, messages = jupyter_kernel.execute_read_reply("(format t \"hello, world\")", timeout = 10)
    print(messages)
    assert any(msg['msg_type'] == 'stream' and msg['content']['name'] == 'stdout'
               and msg['content']['text'] == 'hello, world' for msg in messages), "wibble"


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

