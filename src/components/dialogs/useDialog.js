import { useContext } from 'react'
import DialogContext from './DialogContext'

function useDialog () {
  const { dialog } = useContext(DialogContext)
  return dialog
}

export default useDialog
