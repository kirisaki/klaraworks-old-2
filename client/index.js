'use strict'

import './index.html'
import './scss/style.scss'

import { Elm } from './Main.elm'

const mountNode = document.getElementById('main')
const app = Elm.Main.init({
  node: mountNode
})
