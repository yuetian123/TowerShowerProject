using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Card : MonoBehaviour
{
    public int index;
    private void Awake()
    {
        CardControl.Instance.cards.Add(this);
    }
}
