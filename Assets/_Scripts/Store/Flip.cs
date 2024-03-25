using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Flip : MonoBehaviour
{
    public GameObject item;
    public void FlipItem()
    {
        item.SetActive(!item.active);
    }
}
