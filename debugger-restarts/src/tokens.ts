import { ISignal, Signal } from '@lumino/signaling';

export interface IRestart {

  readonly name: string;

  readonly text: string;

}


export interface IRestartsModel {

  readonly changed: ISignal<this, IRestart[]>;

  readonly clicked: Signal<this, number>;

  restarts: IRestart[];
  threadId: number;
}
