using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class Hint : MonoBehaviour
{
    public GameObject bg;
    public Text dis;
    private Transform player;
    private Transform playerC;

    private void Awake()
    {
        playerC = GameObject.FindGameObjectWithTag("PlayerCamera").transform;
        player = GameObject.FindGameObjectWithTag("Player").transform;
    }
    private void Update()
    {
        if (gameObject.active)
        {
            transform.LookAt(playerC);
            if (((int)Vector3.Distance(player.position,transform.position)).ToString() != dis.text)
            {
                dis.text = ((int)Vector3.Distance(player.position, transform.position)).ToString();
            }
        }
    }
}
