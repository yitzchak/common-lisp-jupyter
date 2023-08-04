import {
  JupyterFrontEnd,
  JupyterFrontEndPlugin
} from '@jupyterlab/application';

import { Kernel, Session } from '@jupyterlab/services';

import { IDebugger, IDebuggerSidebar } from '@jupyterlab/debugger';

import { IRestartsModel } from './tokens';
import { RestartsModel } from './model';
import { RestartsPanel } from './panel';

/**
 * Initialization data for the debugger-restarts-clj extension.
 */
const plugin: JupyterFrontEndPlugin<void> = {
  id: 'debugger-restarts-clj:plugin',
  autoStart: true,
  requires: [IDebugger, IDebuggerSidebar],
  activate: (
    app: JupyterFrontEnd,
    service: IDebugger,
    sidebar: IDebugger.ISidebar
  ) => {
    console.log('debugger-restarts-clj activated.');

    let model: IRestartsModel = new RestartsModel();
    let panel: RestartsPanel = new RestartsPanel({
      model: model
    });

    model.clicked.connect((model: IRestartsModel, restartNumber: number) => {
      if (service.session) {
        service.session.sendRequest('continue', {
          threadId: model.threadId,
          restart: restartNumber,
          frameId: service.model.callstack.frame?.id
        } as any);
      }
    });

    service.eventMessage.connect((service: IDebugger, event: IDebugger.ISession.Event) => {
      switch (event.event) {
        case 'stopped':
          model.threadId = event.body.threadId;
          model.restarts = event.body.restarts || [];
          break;
        case 'continued':
        case 'initialized':
        case 'terminated':
          model.restarts = [];
          break;
      }
    });

    service.sessionChanged.connect((service: IDebugger, session: IDebugger.ISession | null) => {
      model.restarts = [];
      if (session && session.connection) {
        session.connection.kernelChanged.connect((connection: Session.ISessionConnection, args: Session.ISessionConnection.IKernelChangedArgs) => {
          model.restarts = [];
        });
        session.connection.statusChanged.connect((connection: Session.ISessionConnection, status: Kernel.Status) => {
          if (status === 'restarting') {
            model.restarts = [];
          }
        });
      }
    });

    sidebar.insertWidget(2, panel);
  }
};

export default plugin;
