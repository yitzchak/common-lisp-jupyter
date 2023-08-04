import { ISignal, Signal } from '@lumino/signaling';

import { IRestart, IRestartsModel } from './tokens';


export class RestartsModel implements IRestartsModel {

  private _restarts: IRestart[] = [];
  private _threadId: number = 1;
  private _changed = new Signal<this, IRestart[]>(this);
  private _clicked = new Signal<this, number>(this);

  get changed(): ISignal<this, IRestart[]> {
    return this._changed;
  }

  get clicked(): Signal<this, number> {
    return this._clicked;
  }

  get restarts(): IRestart[] {
    return this._restarts;
  }

  set restarts(restarts: IRestart[]) {
    this._restarts = restarts;
    this._changed.emit(restarts);
  }

  get threadId(): number {
    return this._threadId;
  }

  set threadId(threadId: number) {
    this._threadId = threadId;
  }
}
